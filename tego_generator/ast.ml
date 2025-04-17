open! Core
module Ast = Tego_parser.Ast

let random_ident () =
  let max_length = 15 in
  let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'" in
  let alphabet_length = String.length alphabet in
  let random_char () = alphabet.[Random.int alphabet_length] in
  let rec loop () =
    let length = Random.int_incl 1 max_length in
    let result = String.init length ~f:(fun _ -> random_char ()) in
    if String.exists result ~f:(fun c -> not (Char.equal '\'' c)) then result
    else loop ()
  in
  loop ()

let random_string () =
  let max_length = 64 in
  let length = Random.int_incl 0 max_length in
  String.init length ~f:(fun _ -> Random.char ())

let random_number () = Random.int Int.max_value

let random_bool () = Random.bool ()

let random_char () = Random.char ()

type match_value_weights = Weights.MatchValue.t

type match_weights = Weights.Match.t * match_value_weights

type expr_value_weights = Weights.ExprValue.t

type expr_unary_op_weights = Weights.ExprUnaryOp.t

type expr_binary_op_weights = Weights.ExprBinaryOp.t

type expr_weights =
  Weights.Expr.t
  * expr_value_weights
  * expr_unary_op_weights
  * expr_binary_op_weights
  * match_weights

type decl_weights = Weights.Decl.t * expr_weights

module MatchValue : sig
  val generate : match_value_weights -> Ast.Match.value
end = struct
  let rec generate (weights : match_value_weights) : Ast.Match.value =
    let weighted_range =
      Weights.MatchValue.to_weighted_range weights
        ~int:(fun () -> Ast.Match.Int (random_number ()))
        ~bool:(fun () -> Ast.Match.Bool (random_bool ()))
        ~char:(fun () -> Ast.Match.Char (random_char ()))
        ~string:(fun () -> Ast.Match.String (random_ident ()))
    in
    Weights.T.pick_random weighted_range ()
end

module Match : sig
  val generate : match_weights -> Ast.Match.t
end = struct
  let rec generate
      ((match_weights, match_value_weights) as weights : match_weights) :
      Ast.Match.t =
    let open Ast.Match in
    let weighted_range =
      Weights.Match.to_weighted_range match_weights
        ~ident:(fun () ->
          let ident = random_ident () in
          Ident ident )
        ~list:(fun () -> List (generate weights, generate weights))
        ~boxed:(fun () -> Boxed (generate weights))
        ~value:(fun () -> Value (MatchValue.generate match_value_weights))
        ~unit:(fun () -> Unit)
        ~ignore:(fun () -> Ignore)
    in
    Weights.T.pick_random weighted_range ()
end

module ExprValue : sig
  val generate : expr_value_weights -> Ast.Expr.value
end = struct
  let generate expr_value_weights =
    let weighted_range =
      Weights.ExprValue.to_weighted_range expr_value_weights
        ~int:(fun () -> Ast.Expr.Int (random_number ()))
        ~bool:(fun () -> Ast.Expr.Bool (random_bool ()))
        ~char:(fun () -> Ast.Expr.Char (random_char ()))
        ~string:(fun () -> Ast.Expr.String (random_string ()))
        ~unit:(fun () -> Ast.Expr.Unit)
    in
    Weights.T.pick_random weighted_range ()
end

module ExprUnaryOp : sig
  val generate : expr_unary_op_weights -> Ast.Expr.unary_op
end = struct
  let generate expr_unary_op_weights =
    let open Ast.Expr in
    let weighted_range =
      Weights.ExprUnaryOp.to_weighted_range expr_unary_op_weights
        ~not_:(fun () -> Not)
        ~negate:(fun () -> Negate)
    in
    Weights.T.pick_random weighted_range ()
end

module ExprBinaryOp : sig
  val generate : expr_binary_op_weights -> Ast.Expr.binary_op
end = struct
  let generate expr_binary_op_weights =
    let open Ast.Expr in
    let weighted_range =
      Weights.ExprBinaryOp.to_weighted_range expr_binary_op_weights
        ~plus:(fun () -> Plus)
        ~minus:(fun () -> Minus)
        ~multiply:(fun () -> Multiply)
        ~divide:(fun () -> Divide)
        ~modulo:(fun () -> Modulo)
        ~and_:(fun () -> And)
        ~or_:(fun () -> Or)
        ~xor:(fun () -> Xor)
        ~join:(fun () -> Join)
        ~flat_join:(fun () -> FlatJoin)
        ~equal:(fun () -> Equal)
        ~not_equal:(fun () -> NotEqual)
        ~less_than:(fun () -> LessThan)
        ~less_than_or_equal:(fun () -> LessThanEqual)
        ~greater_than:(fun () -> GreaterThan)
        ~greater_than_or_equal:(fun () -> GreaterThanEqual)
    in
    Weights.T.pick_random weighted_range ()
end

module Expr : sig
  val generate : expr_weights -> Ast.Expr.t
end = struct
  let rec generate
      (( expr_weights
       , expr_value_weights
       , expr_unary_op_weights
       , expr_binary_op_weights
       , match_weights ) as weights :
        expr_weights ) : Ast.Expr.t =
    let open Ast.Expr in
    let generate_match_branches () =
      let max_match_branches = 8 in
      let num_branches = Random.int_incl 1 max_match_branches in
      List.init num_branches ~f:(fun _ ->
          (Match.generate match_weights, generate weights) )
    in
    let weighted_range =
      Weights.Expr.to_weighted_range expr_weights
        ~do_:(fun () ->
          Do (generate weights, Match.generate match_weights, generate weights) )
        ~if_:(fun () ->
          If (generate weights, generate weights, generate weights) )
        ~let_:(fun () ->
          Let (Match.generate match_weights, generate weights, generate weights) )
        ~fn:(fun () -> Fn (Match.generate match_weights, generate weights))
        ~fn_app:(fun () -> FnApp (generate weights, generate weights))
        ~match_:(fun () -> Match (generate weights, generate_match_branches ()))
        ~delayed:(fun () ->
          Delayed
            ( Ast.Match.Ident (random_ident ())
            , generate weights
            , generate weights ) )
        ~boxed:(fun () -> Boxed (generate weights))
        ~variable:(fun () -> Variable (random_ident ()))
        ~unary:(fun () ->
          Unary (ExprUnaryOp.generate expr_unary_op_weights, generate weights) )
        ~binary:(fun () ->
          Binary
            ( generate weights
            , ExprBinaryOp.generate expr_binary_op_weights
            , generate weights ) )
        ~literal:(fun () -> Literal (ExprValue.generate expr_value_weights))
    in
    Weights.T.pick_random weighted_range ()
end

module Decl : sig
  val generate : decl_weights -> Ast.Decl.t
end = struct
  let generate ((decl_weights, expr_weights) : decl_weights) =
    let open Ast.Decl in
    let weighted_range =
      Weights.Decl.to_weighted_range decl_weights ~expression:(fun () ->
          Expression (random_ident (), Expr.generate expr_weights) )
    in
    Weights.T.pick_random weighted_range ()
end
