open! Core

module T : sig
  type 'a weighted_range

  val create_weighted_range : ('a * int) list -> 'a weighted_range

  val pick_random : 'a weighted_range -> 'a
end = struct
  type upper_bound = int

  type 'a weighted_range = ('a * upper_bound) list

  let create_weighted_range (weights : ('a * int) list) : 'a weighted_range =
    List.folding_map weights ~init:0 ~f:(fun acc (x, w) ->
        (acc + w, (x, acc + w)) )

  let pick_random (range : 'a weighted_range) : 'a =
    let max = List.last_exn range |> snd in
    let r = Random.int max in
    let rec loop = function
      | [] ->
          failwith "unreachable"
      | (x, upper_bound) :: xs ->
          if r < upper_bound then x else loop xs
    in
    loop range
end

module MatchValue : sig
  type t

  val make : ?int:int -> ?bool:int -> ?char:int -> ?string:int -> unit -> t

  val to_weighted_range :
       t
    -> int:(unit -> 'a)
    -> bool:(unit -> 'a)
    -> char:(unit -> 'a)
    -> string:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t = {int: int; bool: int; char: int; string: int}

  let make ?(int = 1) ?(bool = 1) ?(char = 1) ?(string = 1) () : t =
    {int; bool; char; string}

  let to_weighted_range (t : t) ~(int : unit -> 'a) ~(bool : unit -> 'a)
      ~(char : unit -> 'a) ~(string : unit -> 'a) :
      (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range
      [(int, t.int); (bool, t.bool); (char, t.char); (string, t.string)]
end

module Match : sig
  type t

  val make :
       ?ident:int
    -> ?list:int
    -> ?boxed:int
    -> ?value:int
    -> ?unit:int
    -> ?ignore:int
    -> unit
    -> t

  val to_weighted_range :
       t
    -> ident:(unit -> 'a)
    -> list:(unit -> 'a)
    -> boxed:(unit -> 'a)
    -> value:(unit -> 'a)
    -> unit:(unit -> 'a)
    -> ignore:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t =
    {ident: int; list: int; boxed: int; value: int; unit: int; ignore: int}

  let make ?(ident = 1) ?(list = 1) ?(boxed = 1) ?(value = 1) ?(unit = 1)
      ?(ignore = 1) () : t =
    {ident; list; boxed; value; unit; ignore}

  let to_weighted_range (t : t) ~(ident : unit -> 'a) ~(list : unit -> 'a)
      ~(boxed : unit -> 'a) ~(value : unit -> 'a) ~(unit : unit -> 'a)
      ~(ignore : unit -> 'a) : (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range
      [ (ident, t.ident)
      ; (list, t.list)
      ; (boxed, t.boxed)
      ; (value, t.value)
      ; (unit, t.unit)
      ; (ignore, t.ignore) ]
end

module ExprValue : sig
  type t

  val make :
    ?int:int -> ?bool:int -> ?char:int -> ?string:int -> ?unit:int -> unit -> t

  val to_weighted_range :
       t
    -> int:(unit -> 'a)
    -> bool:(unit -> 'a)
    -> char:(unit -> 'a)
    -> string:(unit -> 'a)
    -> unit:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t = {int: int; bool: int; char: int; string: int; unit: int}

  let make ?(int = 1) ?(bool = 1) ?(char = 1) ?(string = 1) ?(unit = 1) () : t =
    {int; bool; char; string; unit}

  let to_weighted_range (t : t) ~(int : unit -> 'a) ~(bool : unit -> 'a)
      ~(char : unit -> 'a) ~(string : unit -> 'a) ~(unit : unit -> 'a) :
      (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range
      [ (int, t.int)
      ; (bool, t.bool)
      ; (char, t.char)
      ; (string, t.string)
      ; (unit, t.unit) ]
end

module ExprUnaryOp : sig
  type t

  val make : ?not_:int -> ?negate:int -> unit -> t

  val to_weighted_range :
       t
    -> not_:(unit -> 'a)
    -> negate:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t = {not_: int; negate: int}

  let make ?(not_ = 1) ?(negate = 1) () : t = {not_; negate}

  let to_weighted_range (t : t) ~(not_ : unit -> 'a) ~(negate : unit -> 'a) :
      (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range [(not_, t.not_); (negate, t.negate)]
end

module ExprBinaryOp : sig
  type t

  val make :
       ?plus:int
    -> ?minus:int
    -> ?multiply:int
    -> ?divide:int
    -> ?modulo:int
    -> ?and_:int
    -> ?or_:int
    -> ?xor:int
    -> ?join:int
    -> ?flat_join:int
    -> ?equal:int
    -> ?not_equal:int
    -> ?less_than:int
    -> ?less_than_or_equal:int
    -> ?greater_than:int
    -> ?greater_than_or_equal:int
    -> unit
    -> t

  val to_weighted_range :
       t
    -> plus:(unit -> 'a)
    -> minus:(unit -> 'a)
    -> multiply:(unit -> 'a)
    -> divide:(unit -> 'a)
    -> modulo:(unit -> 'a)
    -> and_:(unit -> 'a)
    -> or_:(unit -> 'a)
    -> xor:(unit -> 'a)
    -> join:(unit -> 'a)
    -> flat_join:(unit -> 'a)
    -> equal:(unit -> 'a)
    -> not_equal:(unit -> 'a)
    -> less_than:(unit -> 'a)
    -> less_than_or_equal:(unit -> 'a)
    -> greater_than:(unit -> 'a)
    -> greater_than_or_equal:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t =
    { plus: int
    ; minus: int
    ; multiply: int
    ; divide: int
    ; modulo: int
    ; and_: int
    ; or_: int
    ; xor: int
    ; join: int
    ; flat_join: int
    ; equal: int
    ; not_equal: int
    ; less_than: int
    ; less_than_or_equal: int
    ; greater_than: int
    ; greater_than_or_equal: int }

  let make ?(plus = 1) ?(minus = 1) ?(multiply = 1) ?(divide = 1) ?(modulo = 1)
      ?(and_ = 1) ?(or_ = 1) ?(xor = 1) ?(join = 1) ?(flat_join = 1)
      ?(equal = 1) ?(not_equal = 1) ?(less_than = 1) ?(less_than_or_equal = 1)
      ?(greater_than = 1) ?(greater_than_or_equal = 1) () : t =
    { plus
    ; minus
    ; multiply
    ; divide
    ; modulo
    ; and_
    ; or_
    ; xor
    ; join
    ; flat_join
    ; equal
    ; not_equal
    ; less_than
    ; less_than_or_equal
    ; greater_than
    ; greater_than_or_equal }

  let to_weighted_range (t : t) ~(plus : unit -> 'a) ~(minus : unit -> 'a)
      ~(multiply : unit -> 'a) ~(divide : unit -> 'a) ~(modulo : unit -> 'a)
      ~(and_ : unit -> 'a) ~(or_ : unit -> 'a) ~(xor : unit -> 'a)
      ~(join : unit -> 'a) ~(flat_join : unit -> 'a) ~(equal : unit -> 'a)
      ~(not_equal : unit -> 'a) ~(less_than : unit -> 'a)
      ~(less_than_or_equal : unit -> 'a) ~(greater_than : unit -> 'a)
      ~(greater_than_or_equal : unit -> 'a) : (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range
      [ (plus, t.plus)
      ; (minus, t.minus)
      ; (multiply, t.multiply)
      ; (divide, t.divide)
      ; (modulo, t.modulo)
      ; (and_, t.and_)
      ; (or_, t.or_)
      ; (xor, t.xor)
      ; (join, t.join)
      ; (flat_join, t.flat_join)
      ; (equal, t.equal)
      ; (not_equal, t.not_equal)
      ; (less_than, t.less_than)
      ; (less_than_or_equal, t.less_than_or_equal)
      ; (greater_than, t.greater_than)
      ; (greater_than_or_equal, t.greater_than_or_equal) ]
end

module Expr : sig
  type t

  val make :
       ?do_:int
    -> ?if_:int
    -> ?let_:int
    -> ?fn:int
    -> ?fn_app:int
    -> ?match_:int
    -> ?delayed:int
    -> ?boxed:int
    -> ?variable:int
    -> ?unary:int
    -> ?binary:int
    -> ?literal:int
    -> unit
    -> t

  val to_weighted_range :
       t
    -> do_:(unit -> 'a)
    -> if_:(unit -> 'a)
    -> let_:(unit -> 'a)
    -> fn:(unit -> 'a)
    -> fn_app:(unit -> 'a)
    -> match_:(unit -> 'a)
    -> delayed:(unit -> 'a)
    -> boxed:(unit -> 'a)
    -> variable:(unit -> 'a)
    -> unary:(unit -> 'a)
    -> binary:(unit -> 'a)
    -> literal:(unit -> 'a)
    -> (unit -> 'a) T.weighted_range
end = struct
  type t =
    { do_: int
    ; if_: int
    ; let_: int
    ; fn: int
    ; fn_app: int
    ; match_: int
    ; delayed: int
    ; boxed: int
    ; variable: int
    ; unary: int
    ; binary: int
    ; literal: int }

  let make ?(do_ = 1) ?(if_ = 1) ?(let_ = 1) ?(fn = 1) ?(fn_app = 1)
      ?(match_ = 1) ?(delayed = 1) ?(boxed = 1) ?(variable = 1) ?(unary = 1)
      ?(binary = 1) ?(literal = 1) () : t =
    { do_
    ; if_
    ; let_
    ; fn
    ; fn_app
    ; match_
    ; delayed
    ; boxed
    ; variable
    ; unary
    ; binary
    ; literal }

  let to_weighted_range (t : t) ~(do_ : unit -> 'a) ~(if_ : unit -> 'a)
      ~(let_ : unit -> 'a) ~(fn : unit -> 'a) ~(fn_app : unit -> 'a)
      ~(match_ : unit -> 'a) ~(delayed : unit -> 'a) ~(boxed : unit -> 'a)
      ~(variable : unit -> 'a) ~(unary : unit -> 'a) ~(binary : unit -> 'a)
      ~(literal : unit -> 'a) : (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range
      [ (do_, t.do_)
      ; (if_, t.if_)
      ; (let_, t.let_)
      ; (fn, t.fn)
      ; (fn_app, t.fn_app)
      ; (match_, t.match_)
      ; (delayed, t.delayed)
      ; (boxed, t.boxed)
      ; (variable, t.variable)
      ; (unary, t.unary)
      ; (binary, t.binary)
      ; (literal, t.literal) ]
end

module Decl : sig
  type t

  val make : ?expression:int -> unit -> t

  val to_weighted_range :
    t -> expression:(unit -> 'a) -> (unit -> 'a) T.weighted_range
end = struct
  type t = {expression: int}

  let make ?(expression = 1) () : t = {expression}

  let to_weighted_range (t : t) ~(expression : unit -> 'a) :
      (unit -> 'a) T.weighted_range =
    let open T in
    create_weighted_range [(expression, t.expression)]
end
