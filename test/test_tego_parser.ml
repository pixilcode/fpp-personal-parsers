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
    |> String.concat ~sep:"; "

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

  let tests = "test_match" >::: [atom_test]
end

module Test_expressions = struct
  let tests = "test_expressions" >::: []
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
