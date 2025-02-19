open! Core
module Parser = Fpp.Basic_parser.Make (Int)
open Parser.Infix_ops

let number = Parser.take_while Char.is_digit |> Parser.map ~f:Int.of_string

let add = Parser.char '+' >>* Parser.unit ( + )

let sub = Parser.char '-' >>* Parser.unit ( - )

let mul = Parser.char '*' >>* Parser.unit ( * )

let div = Parser.char '/' >>* Parser.unit ( / )

(*
  EXPR ::= EXPR + TERM
         | EXPR - TERM
         | TERM
  
  TERM ::= TERM * FACTOR
         | TERM / FACTOR
         | FACTOR

  FACTOR ::= NUMBER
           | (EXPR)
*)
let apply_bin_op ((a, op), b) = op a b

let rec expr : int Parser.t =
 fun (idx, callback) ->
  Parser.memo ~tag:"expr"
    (expr <&> (add <|> sub) <&> term >>| apply_bin_op <|> term)
    (idx, callback)

and term : int Parser.t =
 fun (idx, callback) ->
  Parser.memo ~tag:"term"
    (term <&> (mul <|> div) <&> factor >>| apply_bin_op <|> factor)
    (idx, callback)

and factor : int Parser.t =
 fun (idx, callback) ->
  Parser.memo ~tag:"factor"
    (number <|> (Parser.char '(' >>* expr *>> Parser.char ')'))
    (idx, callback)

let parser : int Parser.t = expr
