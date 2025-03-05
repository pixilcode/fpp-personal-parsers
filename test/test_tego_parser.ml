open! Core
open OUnit2
module Match = Tego_parser.Ast.Match
module Expr = Tego_parser.Ast.Expr
module Decl = Tego_parser.Ast.Decl
module Prog = Tego_parser.Ast.Prog

module Test_match = struct
  let parse = Tego_parser.Parser.parse_match

  let match_list_to_string ms =
    List.map ms ~f:(fun m -> Match.sexp_of_t m |> Sexp.to_string_hum)
    |> fun s -> "(" ^ String.concat ~sep:"; " s ^ ")"

  let atom_test =
    "atom_test"
    >:: fun _ ->
    (* boolean true *)
    let input = "true" in
    let result = parse input in
    let expected = [Match.Value (Match.Bool true)] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* boolean false *)
    let input = "false" in
    let result = parse input in
    let expected = [Match.Value (Match.Bool false)] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* ignore *)
    let input = "_" in
    let result = parse input in
    let expected = [Match.Ignore] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* ident *)
    let input = "a" in
    let result = parse input in
    let expected = [Match.Ident "a"] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* int *)
    let input = "1" in
    let result = parse input in
    let expected = [Match.Value (Match.Int 1)] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* string *)
    let input = "\"a\"" in
    let result = parse input in
    let expected = [Match.Value (Match.String "a")] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* char *)
    let input = "'a'" in
    let result = parse input in
    let expected = [Match.Value (Match.Char 'a')] in
    assert_equal expected result ~printer:match_list_to_string

  let grouping_test =
    "grouping_test"
    >:: fun _ ->
    (* parentheses *)
    let input = "(1)" in
    let result = parse input in
    let expected = [Match.Value (Match.Int 1)] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* boxed *)
    let input = "[1]" in
    let result = parse input in
    let expected = [Match.Boxed (Match.Value (Match.Int 1))] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* unit *)
    let input = "()" in
    let result = parse input in
    let expected = [Match.Unit] in
    assert_equal expected result ~printer:match_list_to_string ;
    (* atom *)
    let input = "1" in
    let result = parse input in
    let expected = [Match.Value (Match.Int 1)] in
    assert_equal expected result ~printer:match_list_to_string

  let list_test =
    "list_test"
    >:: fun _ ->
    (* empty list *)
    let input = "a, b" in
    let result = parse input in
    let expected = [Match.List (Match.Ident "a", Match.Ident "b")] in
    assert_equal expected result ~printer:match_list_to_string ;
    let input = "a, b, c" in
    let result = parse input in
    let expected =
      [ Match.List
          (Match.Ident "a", Match.List (Match.Ident "b", Match.Ident "c")) ]
    in
    assert_equal expected result ~printer:match_list_to_string ;
    let input = "1" in
    let result = parse input in
    let expected = [Match.Value (Match.Int 1)] in
    assert_equal expected result ~printer:match_list_to_string

  let tests = "test_match" >::: [atom_test; grouping_test; list_test]
end

module Test_expressions = struct
  let parse = Tego_parser.Parser.parse_expression

  let expr_list_to_string es =
    List.map es ~f:(fun e -> Expr.sexp_of_t e |> Sexp.to_string_hum)
    |> fun s -> "(" ^ String.concat ~sep:"; " s ^ ")"

  let literal_test =
    "literal_test"
    >:: fun _ ->
    (* boolean true *)
    let input = "true" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Bool true)] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* boolean false *)
    let input = "false" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Bool false)] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* variable *)
    let input = "a" in
    let result = parse input in
    let expected = [Expr.Variable "a"] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* integer *)
    let input = "1" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Int 1)] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* string *)
    let input = "\"a\"" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.String "a")] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* char *)
    let input = "'a'" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Char 'a')] in
    assert_equal expected result ~printer:expr_list_to_string

  let grouping_expression_test =
    "grouping_expression_test"
    >:: fun _ ->
    (* parentheses *)
    let input = "(1)" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Int 1)] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* boxed *)
    let input = "[1]" in
    let result = parse input in
    let expected = [Expr.Boxed (Expr.Literal (Expr.Int 1))] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* unit *)
    let input = "()" in
    let result = parse input in
    let expected = [Expr.Literal Expr.Unit] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* literal *)
    let input = "1" in
    let result = parse input in
    let expected = [Expr.Literal (Expr.Int 1)] in
    assert_equal expected result ~printer:expr_list_to_string

  let tests = "test_expressions" >::: [literal_test; grouping_expression_test]
end

module Test_declaration = struct
  let tests = "test_declaration" >::: []
end

module Test_program = struct
  let tests = "test_program" >::: []
end

let () =
  run_test_tt_main
    ( "test_tego_parser"
    >::: [ Test_match.tests
         ; Test_expressions.tests
         ; Test_declaration.tests
         ; Test_program.tests ] )
