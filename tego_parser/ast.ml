open! Core

module Match = struct
  type value = Int of int | Bool of bool | Char of char | String of string
  [@@deriving compare, sexp]

  type t =
    | Ident of string
    | List of t list
    | Boxed of t
    | Value of value
    | Unit
    | Ignore
  [@@deriving compare, sexp]

  let join (a : t) (b : t) : t =
    match (a, b) with
    | List a, List b ->
        List (a @ b)
    | List a, b ->
        List (a @ [b])
    | a, List b ->
        List (a :: b)
    | a, b ->
        List [a; b]

  let rec to_debug_string (m : t) : string =
    match m with
    | Ident s ->
        s
    | List l ->
        let list = List.map ~f:to_debug_string l in
        let list_str = String.concat ~sep:", " list in
        "(" ^ list_str ^ ")"
    | Boxed m ->
        "[" ^ to_debug_string m ^ "]"
    | Value v -> (
      match v with
      | Int i ->
          string_of_int i
      | Bool b ->
          string_of_bool b
      | Char c ->
          "'" ^ Char.escaped c ^ "'"
      | String s ->
          "\"" ^ s ^ "\"" )
    | Unit ->
        "()"
    | Ignore ->
        "_"
end

module Expr = struct
  type value =
    | Int of int
    | Bool of bool
    | Char of char
    | String of string
    | Unit
  [@@deriving compare, sexp]

  type unary_op = Not | Negate [@@deriving compare, sexp]

  type binary_op =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | And
    | Or
    | Xor
    | Join
    | FlatJoin
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessThanEqual
    | GreaterThanEqual
  [@@deriving compare, sexp]

  type t =
    | Do of t * Match.t * t
    | If of t * t * t
    | Let of Match.t * t * t
    | Fn of Match.t * t
    | FnApp of t * t
    | Match of t * (Match.t * t) list
    | Delayed of Match.t * t * t
    | Boxed of t
    | Variable of string
    | Unary of unary_op * t
    | Binary of t * binary_op * t
    | Literal of value
  [@@deriving compare, sexp]

  let rec to_debug_string (e : t) : string =
    match e with
    | Do (e1, m, e2) ->
        "do " ^ to_debug_string e1 ^ " in " ^ Match.to_debug_string m ^ " then "
        ^ to_debug_string e2
    | If (e1, e2, e3) ->
        "if " ^ to_debug_string e1 ^ " then " ^ to_debug_string e2 ^ " else "
        ^ to_debug_string e3
    | Let (m, e1, e2) ->
        "let " ^ Match.to_debug_string m ^ " = " ^ to_debug_string e1 ^ " in "
        ^ to_debug_string e2
    | Fn (m, e) ->
        "fn " ^ Match.to_debug_string m ^ " -> " ^ to_debug_string e
    | FnApp (e1, e2) ->
        to_debug_string e1 ^ " " ^ to_debug_string e2
    | Match (e, l) ->
        let cases =
          List.map
            ~f:(fun (m, e) ->
              " | " ^ Match.to_debug_string m ^ " -> " ^ to_debug_string e )
            l
        in
        "match " ^ to_debug_string e ^ " with " ^ String.concat cases
    | Delayed (m, e1, e2) ->
        "delay " ^ Match.to_debug_string m ^ " = " ^ to_debug_string e1 ^ " in "
        ^ to_debug_string e2
    | Boxed e ->
        "[" ^ to_debug_string e ^ "]"
    | Variable s ->
        s
    | Unary (op, e) ->
        (match op with Not -> "not " | Negate -> "-") ^ to_debug_string e
    | Binary (e1, op, e2) ->
        to_debug_string e1 ^ " "
        ^ ( match op with
          | Plus ->
              "+"
          | Minus ->
              "-"
          | Multiply ->
              "*"
          | Divide ->
              "/"
          | Modulo ->
              "%"
          | And ->
              "and"
          | Or ->
              "or"
          | Xor ->
              "xor"
          | Join ->
              ","
          | FlatJoin ->
              ",,"
          | Equal ->
              "=="
          | NotEqual ->
              "/="
          | LessThan ->
              "<"
          | GreaterThan ->
              ">"
          | LessThanEqual ->
              "<="
          | GreaterThanEqual ->
              ">=" )
        ^ " " ^ to_debug_string e2
    | Literal v -> (
      match v with
      | Int i ->
          string_of_int i
      | Bool b ->
          string_of_bool b
      | Char c ->
          "'" ^ Char.escaped c ^ "'"
      | String s ->
          "\"" ^ s ^ "\""
      | Unit ->
          "()" )
end

module Decl = struct
  type t = Expression of string * Expr.t [@@deriving compare, sexp]

  let to_debug_string (d : t) : string =
    match d with Expression (s, e) -> s ^ " = " ^ Expr.to_debug_string e
end

module Prog = struct
  type t = Decl.t list [@@deriving compare, sexp]

  let to_debug_string (p : t) : string =
    let decls = List.map ~f:Decl.to_debug_string p in
    String.concat ~sep:"\n" decls
end
