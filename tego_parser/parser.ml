open! Core

module CachedValue : sig
  type t =
    | MatchNode of Ast.Match.t
    | ExprNode of Ast.Expr.t
    | DeclNode of Ast.Decl.t
    | ProgNode of Ast.Prog.t
  [@@deriving compare, sexp]

  include Fpp.Parser_base.CacheValue with type t := t
end = struct
  open Ast

  type t =
    | MatchNode of Match.t
    | ExprNode of Expr.t
    | DeclNode of Decl.t
    | ProgNode of Prog.t
  [@@deriving compare, sexp]
end

module Parser = Fpp.Basic_parser.Make (CachedValue)

module Token = struct
  open Parser
  open Parser.Infix_ops

  (* whitespace *)
  let inline_whitespace : char parser =
    Strings.char_where (function ' ' | '\t' | '\r' -> true | _ -> false)

  let newline_whitespace : char parser = Strings.char '\n'

  let single_line_comment : string parser =
    Strings.string "--"
    >>* Strings.take_while (fun c -> not (Char.equal c '\n'))
        *>> newline_whitespace
    >>= fun comment -> unit ("--" ^ comment)

  let inline_comment : string parser =
    let valid_comment_chars : string parser =
      many
        ( char_where (fun c -> not (Char.equal c '-' || Char.equal c '\n'))
        <|> char '-' *>> peek (char_where (fun c -> not (Char.equal c '}'))) )
      >>| String.of_char_list
    in
    string "{-"
    >>* valid_comment_chars *>> string "-}"
    >>= fun comment -> unit ("{-" ^ comment ^ "-}")

  let multiline_comment : string parser =
    let valid_comment_chars : string parser =
      many
        ( char_where (fun c -> not (Char.equal c '-'))
        <|> char '-' *>> peek (char_where (fun c -> not (Char.equal c '}'))) )
      >>| String.of_char_list
    in
    Strings.string "{-"
    >>* valid_comment_chars *>> newline_whitespace
    >>* valid_comment_chars *>> Strings.string "-}"
    >>= fun comment -> unit ("{-" ^ comment ^ "-}")

  let inline_ignored : string parser =
    inline_whitespace >>| Char.to_string <|> inline_comment

  let newline_ignored : string parser =
    newline_whitespace >>| Char.to_string <|> single_line_comment
    <|> multiline_comment

  (* whitespace abbreviations *)
  let opt_nl : unit parser =
    many inline_ignored
    >>* many (newline_ignored >>* many inline_ignored)
    >>* unit ()

  let req_nl : unit parser =
    many inline_ignored
    >>* many1 (newline_ignored >>* many inline_ignored)
    >>* unit ()

  (* keywords *)
  let keywords : string list =
    [ "and"
    ; "or"
    ; "xor"
    ; "not"
    ; "true"
    ; "false"
    ; "if"
    ; "then"
    ; "else"
    ; "let"
    ; "in"
    ; "fn"
    ; "match"
    ; "to"
    ; "delay"
    ; "do"
    ; "def" ]

  let and_ : string parser = Strings.string "and"

  let or_ : string parser = Strings.string "or"

  let xor : string parser = Strings.string "xor"

  let not_ : string parser = Strings.string "not"

  let true_ : string parser = Strings.string "true"

  let false_ : string parser = Strings.string "false"

  let if_ : string parser = Strings.string "if"

  let then_ : string parser = Strings.string "then"

  let else_ : string parser = Strings.string "else"

  let let_ : string parser = Strings.string "let"

  let in_ : string parser = Strings.string "in"

  let fn : string parser = Strings.string "fn"

  let match_ : string parser = Strings.string "match"

  let to_ : string parser = Strings.string "to"

  let delay : string parser = Strings.string "delay"

  let do_ : string parser = Strings.string "do"

  let def : string parser = Strings.string "def"

  (* operators *)

  let comma : string parser = Strings.string ","

  let plus : string parser = Strings.string "+"

  let minus : string parser = Strings.string "-"

  let star : string parser = Strings.string "*"

  let slash : string parser = Strings.string "/"

  let modulo : string parser = Strings.string "%"

  let equal : string parser = Strings.string "=="

  let not_equal : string parser = Strings.string "/="

  let less_than : string parser = Strings.string "<"

  let greater_than : string parser = Strings.string ">"

  let less_than_equal : string parser = Strings.string "<="

  let greater_than_equal : string parser = Strings.string ">="

  let left_paren : string parser = Strings.string "("

  let right_paren : string parser = Strings.string ")"

  let q_mark : string parser = Strings.string "?"

  let assign : string parser = Strings.string "="

  let arrow : string parser = Strings.string "->"

  let bar : string parser = Strings.string "|"

  let underscore : string parser = Strings.string "_"

  let left_bracket : string parser = Strings.string "["

  let right_bracket : string parser = Strings.string "]"

  let double_comma : string parser = Strings.string ",,"

  let dot : string parser = Strings.string "."

  (* literals *)

  let identifier : string parser =
    Strings.char_where (function
      | 'a' .. 'z' | 'A' .. 'Z' ->
          true
      | _ ->
          false )
    >>= fun first_char ->
    Strings.take_while (function
      | 'a' .. 'z' | 'A' .. 'Z' | '\'' ->
          true
      | _ ->
          false )
    >>= fun rest ->
    let first_char = Char.to_string first_char in
    let identifier = String.append first_char rest in
    if List.mem keywords identifier ~equal:String.equal then fail
    else unit identifier

  let integer : int parser =
    Strings.char_where (function '0' .. '9' -> true | _ -> false)
    >>= fun first_digit ->
    Strings.take_while (function '0' .. '9' -> true | _ -> false)
    >>= fun rest ->
    if Char.equal first_digit '0' then
      if String.is_empty rest then unit 0 else fail
    else
      let first_digit = Char.to_string first_digit in
      String.append first_digit rest |> Int.of_string |> unit

  let escape_char : char parser =
    Strings.char '\\'
    >>* Strings.char_where (function
          | 'n' | 't' | '"' | '\'' | '\\' ->
              true
          | _ ->
              false )
    >>| function
    | 'n' ->
        '\n'
    | 't' ->
        '\t'
    | '"' ->
        '"'
    | '\'' ->
        '\''
    | '\\' ->
        '\\'
    | _ ->
        failwith "unreachable"

  let inner_string : string parser =
    many
      ( Strings.char_where (function '"' | '\\' -> false | _ -> true)
      <|> escape_char )
    >>| String.of_char_list

  let string : string parser =
    Strings.char '"' >>* inner_string *>> Strings.char '"'

  let inner_char : char parser =
    Strings.char_where (function '\'' | '\\' -> false | _ -> true)
    <|> escape_char

  let char : char parser =
    Strings.char '\'' >>* inner_char *>> Strings.char '\''

  let boolean : bool parser = true_ >>* unit true <|> (false_ >>* unit false)
end

module Match : sig
  val pattern : Ast.Match.t Parser.parser

  val variable : Ast.Match.t Parser.parser
end = struct
  open Parser
  open Parser.Infix_ops
  open Token
  module Match = Ast.Match

  let rec pattern : Match.t parser =
   fun (idx, callback) ->
    ( memo ~tag:"pattern" (list >>| fun m -> CachedValue.MatchNode m)
    >>| fun m ->
    match m with CachedValue.MatchNode m -> m | _ -> failwith "unreachable" )
      (idx, callback)

  and list : Match.t parser =
   fun (idx, callback) ->
    ( grouping
    >>= fun lhs ->
    opt (opt_nl >>* comma >>* opt_nl >>* list)
    >>| fun rhs ->
    match rhs with None -> lhs | Some rhs -> Match.List (lhs, rhs) )
      (idx, callback)

  and grouping : Match.t parser =
   fun (idx, callback) ->
    ( left_paren >>* opt_nl
    >>* opt pattern *>> opt_nl *>> right_paren
    >>| (fun m -> Option.value ~default:Match.Unit m)
    <|> ( left_bracket >>* opt_nl
        >>* pattern *>> opt_nl *>> right_bracket
        >>| fun m -> Match.Boxed m )
    <|> atom )
      (idx, callback)

  and atom : Match.t parser =
   fun (idx, callback) ->
    ( boolean
    >>| (fun b -> Match.Value (Match.Bool b))
    <|> (underscore >>| fun _ -> Match.Ignore)
    <|> (identifier >>| fun id -> Match.Ident id)
    <|> (integer >>| fun i -> Match.Value (Match.Int i))
    <|> (string >>| fun s -> Match.Value (Match.String s))
    <|> (char >>| fun c -> Match.Value (Match.Char c)) )
      (idx, callback)

  let variable : Match.t parser = identifier >>| fun id -> Match.Ident id
end

module Expression : sig
  val expression : Ast.Expr.t Parser.parser
end = struct
  open Parser
  open Parser.Infix_ops
  open Token
  module Expr = Ast.Expr

  let memoize_expression ~(tag : tag) (f : Expr.t parser) : Expr.t parser =
    memo ~tag (f >>| fun e -> CachedValue.ExprNode e)
    >>| function CachedValue.ExprNode e -> e | _ -> failwith "unreachable"

  let rec expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      (memoize_expression ~tag:"expression" do_expression)
      (idx, callback)

  and do_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"do_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( do_ >>* opt_nl >>* expression
      >>= (fun e1 ->
      opt_nl >>* in_ >>* opt_nl >>* Match.pattern
      >>= fun p ->
      opt_nl >>* then_ >>* opt_nl >>* expression
      >>| fun e2 -> Expr.Do (e1, p, e2) )
      <|> let_expression )
      (idx, callback)

  and let_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"let_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( let_ >>* opt_nl >>* Match.pattern
      >>= (fun p ->
      opt_nl >>* assign >>* opt_nl >>* expression
      >>= fun e1 ->
      opt_nl >>* in_ >>* opt_nl >>* expression >>| fun e2 -> Expr.Let (p, e1, e2) )
      <|> ( delay >>* opt_nl >>* Match.variable
          >>= fun v ->
          opt_nl >>* assign >>* opt_nl >>* expression
          >>= fun e1 ->
          opt_nl >>* in_ >>* opt_nl >>* expression
          >>| fun e2 -> Expr.Delayed (v, e1, e2) )
      <|> if_expression )
      (idx, callback)

  and if_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"if_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( if_ >>* opt_nl >>* expression
      >>= (fun e1 ->
      opt_nl >>* (then_ <|> q_mark) >>* opt_nl >>* expression
      >>= fun e2 ->
      opt_nl >>* else_ >>* opt_nl >>* expression
      >>| fun e3 -> Expr.If (e1, e2, e3) )
      <|> match_expression )
      (idx, callback)

  and match_expression : Expr.t parser =
   fun (idx, callback) ->
    let match_arm : (Ast.Match.t * Expr.t) parser =
      bar >>* opt_nl >>* Match.pattern
      >>= fun p -> opt_nl >>* arrow >>* opt_nl >>* expression >>| fun e -> (p, e)
    in
    trace ~tag:"match_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( match_ >>* opt_nl >>* expression
      >>= (fun e ->
      opt_nl >>* to_ >>* opt_nl >>* match_arm
      >>= fun arm1 ->
      many (opt_nl >>* match_arm) >>| fun arms -> Expr.Match (e, arm1 :: arms) )
      <|> join_expression )
      (idx, callback)

  and join_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"join_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( flat_join_expression
      >>= fun e1 ->
      opt (opt_nl >>* comma >>* opt_nl >>* join_expression)
      >>| function None -> e1 | Some e2 -> Expr.Binary (e1, Expr.Join, e2) )
      (idx, callback)

  and flat_join_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"flat_join_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      ( or_expression
      >>= fun e1 ->
      opt (opt_nl >>* double_comma >>* opt_nl >>* flat_join_expression)
      >>| function None -> e1 | Some e2 -> Expr.Binary (e1, Expr.FlatJoin, e2)
      )
      (idx, callback)

  and or_expression : Expr.t parser =
   fun (idx, callback) ->
    trace ~tag:"or_expression"
      ~additional_msg:(idx |> Idx.sexp_of_t |> Sexp.to_string_hum)
      (memoize_expression ~tag:"or_expression"
         ( opt (or_expression *>> opt_nl *>> or_ *>> opt_nl)
         >>= fun e1 ->
         xor_expression
         >>| fun e2 ->
         match e1 with None -> e2 | Some e1 -> Expr.Binary (e1, Expr.Or, e2) ) )
      (idx, callback)

  and xor_expression : Expr.t parser =
   fun (idx, callback) ->
    memoize_expression ~tag:"xor_expression"
      ( opt (xor_expression *>> opt_nl *>> xor *>> opt_nl)
      >>= fun e1 ->
      and_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some e1 -> Expr.Binary (e1, Expr.Xor, e2) )
      (idx, callback)

  and and_expression : Expr.t parser =
   fun (idx, callback) ->
    memoize_expression ~tag:"and_expression"
      ( opt (and_expression *>> opt_nl *>> and_ *>> opt_nl)
      >>= fun e1 ->
      equality_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some e1 -> Expr.Binary (e1, Expr.And, e2) )
      (idx, callback)

  and equality_expression : Expr.t parser =
   fun (idx, callback) ->
    let equal = equal >>* unit Expr.Equal in
    let not_equal = not_equal >>* unit Expr.NotEqual in
    memoize_expression ~tag:"equality_expression"
      ( opt
          (equality_expression <&> (opt_nl >>* (equal <|> not_equal) *>> opt_nl))
      >>= fun e1 ->
      relational_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some (e1, op) -> Expr.Binary (e1, op, e2) )
      (idx, callback)

  and relational_expression : Expr.t parser =
   fun (idx, callback) ->
    let less_than = less_than >>* unit Expr.LessThan in
    let greater_than = greater_than >>* unit Expr.GreaterThan in
    let less_than_equal = less_than_equal >>* unit Expr.LessThanEqual in
    let greater_than_equal =
      greater_than_equal >>* unit Expr.GreaterThanEqual
    in
    memoize_expression ~tag:"relational_expression"
      ( opt
          ( relational_expression
          <&> ( opt_nl
              >>* ( less_than <|> greater_than <|> less_than_equal
                  <|> greater_than_equal )
                  *>> opt_nl ) )
      >>= fun e1 ->
      additive_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some (e1, op) -> Expr.Binary (e1, op, e2) )
      (idx, callback)

  and additive_expression : Expr.t parser =
   fun (idx, callback) ->
    let plus = plus >>* unit Expr.Plus in
    let minus = minus >>* unit Expr.Minus in
    memoize_expression ~tag:"additive_expression"
      ( opt (additive_expression <&> (opt_nl >>* (plus <|> minus) *>> opt_nl))
      >>= fun e1 ->
      multiplicative_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some (e1, op) -> Expr.Binary (e1, op, e2) )
      (idx, callback)

  and multiplicative_expression : Expr.t parser =
   fun (idx, callback) ->
    let star = star >>* unit Expr.Multiply in
    let slash = slash >>* unit Expr.Divide in
    let modulo = modulo >>* unit Expr.Modulo in
    memoize_expression ~tag:"multiplicative_expression"
      ( opt
          ( multiplicative_expression
          <&> (opt_nl >>* (star <|> slash <|> modulo) *>> opt_nl) )
      >>= fun e1 ->
      negate_expression
      >>| fun e2 ->
      match e1 with None -> e2 | Some (e1, op) -> Expr.Binary (e1, op, e2) )
      (idx, callback)

  and negate_expression : Expr.t parser =
   fun (idx, callback) ->
    ( minus >>* opt_nl >>* negate_expression
    >>| (fun e -> Expr.Unary (Expr.Negate, e))
    <|> not_expression )
      (idx, callback)

  and not_expression : Expr.t parser =
   fun (idx, callback) ->
    ( not_ >>* opt_nl >>* not_expression
    >>| (fun e -> Expr.Unary (Expr.Not, e))
    <|> function_expression )
      (idx, callback)

  and function_expression : Expr.t parser =
   fun (idx, callback) ->
    ( fn >>* opt_nl >>* Match.pattern
    >>= (fun p ->
    opt_nl >>* arrow >>* opt_nl >>* expression >>| fun e -> Expr.Fn (p, e) )
    <|> function_application_expression )
      (idx, callback)

  and function_application_expression : Expr.t parser =
   fun (idx, callback) ->
    memoize_expression ~tag:"function_application_expression"
      ( function_application_expression
      >>= fun e1 -> dot_expression >>| fun e2 -> Expr.FnApp (e1, e2) )
      (idx, callback)

  and dot_expression : Expr.t parser =
   fun (idx, callback) ->
    memoize_expression ~tag:"dot_expression"
      ( opt (dot_expression *>> opt_nl *>> dot *>> opt_nl)
      >>= fun e1 ->
      grouping_expression
      >>| fun e2 -> match e1 with None -> e2 | Some e1 -> Expr.FnApp (e1, e2) )
      (idx, callback)

  and grouping_expression : Expr.t parser =
   fun (idx, callback) ->
    ( left_paren >>* opt_nl
    >>* expression *>> opt_nl *>> right_paren
    <|> (left_bracket >>* opt_nl >>* expression *>> opt_nl *>> right_bracket)
    <|> literal_expression )
      (idx, callback)

  and literal_expression : Expr.t parser =
   fun (idx, callback) ->
    ( boolean
    >>| (fun b -> Expr.Literal (Expr.Bool b))
    <|> (identifier >>| fun id -> Expr.Variable id)
    <|> (integer >>| fun i -> Expr.Literal (Expr.Int i))
    <|> (string >>| fun s -> Expr.Literal (Expr.String s))
    <|> (char >>| fun c -> Expr.Literal (Expr.Char c)) )
      (idx, callback)
end

module Declaration : sig
  val declaration : Ast.Decl.t Parser.parser
end = struct
  open Parser
  open Parser.Infix_ops
  open Token
  module Decl = Ast.Decl

  let rec declaration : Decl.t parser =
   fun (idx, callback) ->
    ( memo ~tag:"declaration"
        (expression_declaration >>| fun d -> CachedValue.DeclNode d)
    >>| fun d ->
    match d with CachedValue.DeclNode d -> d | _ -> failwith "unreachable" )
      (idx, callback)

  and expression_declaration : Decl.t parser =
   fun (idx, callback) ->
    ( def >>* opt_nl >>* identifier
    >>= fun id ->
    many (opt_nl >>* Match.pattern)
    >>= fun ps ->
    opt_nl >>* assign >>* opt_nl >>* Expression.expression
    >>| fun e ->
    let e = List.fold_right ps ~init:e ~f:(fun p e -> Ast.Expr.Fn (p, e)) in
    Decl.Expression (id, e) )
      (idx, callback)
end

module Program : sig
  val program : Ast.Prog.t Parser.parser
end = struct
  open Parser
  open Parser.Infix_ops
  open Token
  module Prog = Ast.Prog

  let program : Prog.t parser =
   fun (idx, callback) ->
    ( memo ~tag:"program"
        ( opt_nl
        >>* many Declaration.declaration *>> opt_nl
        >>| fun p -> CachedValue.ProgNode p )
    >>| fun p ->
    match p with CachedValue.ProgNode p -> p | _ -> failwith "unreachable" )
      (idx, callback)
end

let parse_match (input : string) : Ast.Match.t list =
  Parser.run Match.pattern input

let parse_expression (input : string) : Ast.Expr.t list =
  Parser.run Expression.expression input

let parse_declaration (input : string) : Ast.Decl.t list =
  Parser.run Declaration.declaration input

let parse_program (input : string) : Ast.Prog.t list =
  Parser.run Program.program input

let parse_any (input : string) : CachedValue.t list =
  let open Parser.Infix_ops in
  let parser =
    Match.pattern
    >>| (fun m -> CachedValue.MatchNode m)
    <|> (Expression.expression >>| fun e -> CachedValue.ExprNode e)
    <|> (Declaration.declaration >>| fun d -> CachedValue.DeclNode d)
    <|> (Program.program >>| fun p -> CachedValue.ProgNode p)
  in
  Parser.run parser input
