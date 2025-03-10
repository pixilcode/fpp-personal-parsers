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

  let dot_expression_test =
    "dot_expression_test"
    >:: fun _ ->
    (* dot *)
    let input = "a.b" in
    let result = parse input in
    let expected = [Expr.FnApp (Expr.Variable "b", Expr.Variable "a")] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* nested dot *)
    let input = "a.b.c" in
    let result = parse input in
    let expected =
      [ Expr.FnApp
          (Expr.Variable "c", Expr.FnApp (Expr.Variable "b", Expr.Variable "a"))
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* precendence *)
    let input = "a.(b.c)" in
    let result = parse input in
    let expected =
      [ Expr.FnApp
          (Expr.FnApp (Expr.Variable "c", Expr.Variable "b"), Expr.Variable "a")
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let function_application_expression_test =
    "function_application_expression_test"
    >:: fun _ ->
    (* function application *)
    let input = "a b" in
    let result = parse input in
    let expected = [Expr.FnApp (Expr.Variable "a", Expr.Variable "b")] in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* nested function application *)
    let input = "a b c" in
    let result = parse input in
    let expected =
      [ Expr.FnApp
          (Expr.FnApp (Expr.Variable "a", Expr.Variable "b"), Expr.Variable "c")
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    (* dot_expression *)
    let input = "a b.c" in
    let result = parse input in
    let expected =
      [ Expr.FnApp
          (Expr.Variable "a", Expr.FnApp (Expr.Variable "c", Expr.Variable "b"))
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let function_expression_test =
    "function_expression_test"
    >:: fun _ ->
    let input = "fn x -> 1" in
    let result = parse input in
    let expected = [Expr.Fn (Match.Ident "x", Expr.Literal (Expr.Int 1))] in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "fn f -> f 1" in
    let result = parse input in
    let expected =
      [ Expr.Fn
          ( Match.Ident "f"
          , Expr.FnApp (Expr.Variable "f", Expr.Literal (Expr.Int 1)) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "f (fn x -> x)" in
    let result = parse input in
    let expected =
      [ Expr.FnApp
          (Expr.Variable "f", Expr.Fn (Match.Ident "x", Expr.Variable "x")) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let not_expression_test =
    "not_expression_test"
    >:: fun _ ->
    let input = "not true" in
    let result = parse input in
    let expected = [Expr.Unary (Expr.Not, Expr.Literal (Expr.Bool true))] in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "not not true" in
    let result = parse input in
    let expected =
      [ Expr.Unary
          (Expr.Not, Expr.Unary (Expr.Not, Expr.Literal (Expr.Bool true))) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let negate_expression_test =
    "negate_expression_test"
    >:: fun _ ->
    let input = "-1" in
    let result = parse input in
    let expected = [Expr.Unary (Expr.Negate, Expr.Literal (Expr.Int 1))] in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "- -1" in
    let result = parse input in
    let expected =
      [ Expr.Unary
          (Expr.Negate, Expr.Unary (Expr.Negate, Expr.Literal (Expr.Int 1))) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let multiplicative_expression_test =
    "multiplicative_expression_test"
    >:: fun _ ->
    let input = "1 * 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Multiply, Expr.Literal (Expr.Int 2))
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 / 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Divide, Expr.Literal (Expr.Int 2)) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 % 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Modulo, Expr.Literal (Expr.Int 2)) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 * 2 / 3 % 4" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Binary
              ( Expr.Binary
                  ( Expr.Literal (Expr.Int 1)
                  , Expr.Multiply
                  , Expr.Literal (Expr.Int 2) )
              , Expr.Divide
              , Expr.Literal (Expr.Int 3) )
          , Expr.Modulo
          , Expr.Literal (Expr.Int 4) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let additive_expression_test =
    "additive_expression_tests"
    >:: fun _ ->
    let input = "1 + 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Plus, Expr.Literal (Expr.Int 2)) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 - 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Minus, Expr.Literal (Expr.Int 2)) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 + 2 - 3" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Binary
              (Expr.Literal (Expr.Int 1), Expr.Plus, Expr.Literal (Expr.Int 2))
          , Expr.Minus
          , Expr.Literal (Expr.Int 3) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let relational_expression_test =
    "relational_expression_test"
    >:: fun _ ->
    let input = "1 < 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.LessThan, Expr.Literal (Expr.Int 2))
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 <= 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Int 1)
          , Expr.LessThanEqual
          , Expr.Literal (Expr.Int 2) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 > 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Int 1)
          , Expr.GreaterThan
          , Expr.Literal (Expr.Int 2) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 >= 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Int 1)
          , Expr.GreaterThanEqual
          , Expr.Literal (Expr.Int 2) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let equality_expression_test =
    "equality_expression_test"
    >:: fun _ ->
    let input = "1 == 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.Equal, Expr.Literal (Expr.Int 2)) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "1 /= 2" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          (Expr.Literal (Expr.Int 1), Expr.NotEqual, Expr.Literal (Expr.Int 2))
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let and_expression_test =
    "and_expression_test"
    >:: fun _ ->
    let input = "true and false" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Bool true)
          , Expr.And
          , Expr.Literal (Expr.Bool false) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let xor_expression_test =
    "xor_expression_test"
    >:: fun _ ->
    let input = "true xor false" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Bool true)
          , Expr.Xor
          , Expr.Literal (Expr.Bool false) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let or_expression_test =
    "or_expression_test"
    >:: fun _ ->
    let input = "true or false" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Bool true)
          , Expr.Or
          , Expr.Literal (Expr.Bool false) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let boolean_precedence_test =
    "boolean_precedence_test"
    >:: fun _ ->
    let input = "true and false or true" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Binary
              ( Expr.Literal (Expr.Bool true)
              , Expr.And
              , Expr.Literal (Expr.Bool false) )
          , Expr.Or
          , Expr.Literal (Expr.Bool true) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "true or false and true" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Literal (Expr.Bool true)
          , Expr.Or
          , Expr.Binary
              ( Expr.Literal (Expr.Bool false)
              , Expr.And
              , Expr.Literal (Expr.Bool true) ) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "true and false or true and false" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Binary
              ( Expr.Literal (Expr.Bool true)
              , Expr.And
              , Expr.Literal (Expr.Bool false) )
          , Expr.Or
          , Expr.Binary
              ( Expr.Literal (Expr.Bool true)
              , Expr.And
              , Expr.Literal (Expr.Bool false) ) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "true xor false or true xor false" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Binary
              ( Expr.Literal (Expr.Bool true)
              , Expr.Xor
              , Expr.Literal (Expr.Bool false) )
          , Expr.Or
          , Expr.Binary
              ( Expr.Literal (Expr.Bool true)
              , Expr.Xor
              , Expr.Literal (Expr.Bool false) ) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let flat_join_expression_test =
    "flat_join_expression_test"
    >:: fun _ ->
    let input = "a ,, b" in
    let result = parse input in
    let expected =
      [Expr.Binary (Expr.Variable "a", Expr.FlatJoin, Expr.Variable "b")]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "a ,, b ,, c" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Variable "a"
          , Expr.FlatJoin
          , Expr.Binary (Expr.Variable "b", Expr.FlatJoin, Expr.Variable "c") )
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let join_expression_test =
    "join_expression_test"
    >:: fun _ ->
    let input = "a, b" in
    let result = parse input in
    let expected =
      [Expr.Binary (Expr.Variable "a", Expr.Join, Expr.Variable "b")]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "a, b, c" in
    let result = parse input in
    let expected =
      [ Expr.Binary
          ( Expr.Variable "a"
          , Expr.Join
          , Expr.Binary (Expr.Variable "b", Expr.Join, Expr.Variable "c") ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let match_expression_test =
    "match_expression_test"
    >:: fun _ ->
    let input = "match a to | 1 -> 1" in
    let result = parse input in
    let expected =
      [ Expr.Match
          ( Expr.Variable "a"
          , [(Match.Value (Match.Int 1), Expr.Literal (Expr.Int 1))] ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "match a to | 1 -> 1 | 2 -> 2" in
    let result = parse input in
    let expected =
      [ Expr.Match
          ( Expr.Variable "a"
          , [ (Match.Value (Match.Int 1), Expr.Literal (Expr.Int 1))
            ; (Match.Value (Match.Int 2), Expr.Literal (Expr.Int 2)) ] ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let if_expression_test =
    "if_expression_test"
    >:: fun _ ->
    let input = "if true then 1 else 2" in
    let result = parse input in
    let expected =
      [ Expr.If
          ( Expr.Literal (Expr.Bool true)
          , Expr.Literal (Expr.Int 1)
          , Expr.Literal (Expr.Int 2) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "if true then 1 else if false then 2 else 3" in
    let result = parse input in
    let expected =
      [ Expr.If
          ( Expr.Literal (Expr.Bool true)
          , Expr.Literal (Expr.Int 1)
          , Expr.If
              ( Expr.Literal (Expr.Bool false)
              , Expr.Literal (Expr.Int 2)
              , Expr.Literal (Expr.Int 3) ) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "if true ? 1 else 2" in
    let result = parse input in
    let expected =
      [ Expr.If
          ( Expr.Literal (Expr.Bool true)
          , Expr.Literal (Expr.Int 1)
          , Expr.Literal (Expr.Int 2) ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let let_expression_test =
    "let_expression_test"
    >:: fun _ ->
    let input = "let a = 1 in a" in
    let result = parse input in
    let expected =
      [Expr.Let (Match.Ident "a", Expr.Literal (Expr.Int 1), Expr.Variable "a")]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "let a = 1 in let b = 2 in a" in
    let result = parse input in
    let expected =
      [ Expr.Let
          ( Match.Ident "a"
          , Expr.Literal (Expr.Int 1)
          , Expr.Let
              (Match.Ident "b", Expr.Literal (Expr.Int 2), Expr.Variable "a") )
      ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "delay a = 1 in a" in
    let result = parse input in
    let expected =
      [ Expr.Delayed
          (Match.Ident "a", Expr.Literal (Expr.Int 1), Expr.Variable "a") ]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let do_expression_test =
    "do_expression_test"
    >:: fun _ ->
    let input = "do a then b" in
    let result = parse input in
    let expected =
      [Expr.Do (Expr.Variable "a", Match.Ignore, Expr.Variable "b")]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "do a then do b then c" in
    let result = parse input in
    let expected =
      [ Expr.Do
          ( Expr.Variable "a"
          , Match.Ignore
          , Expr.Do (Expr.Variable "b", Match.Ignore, Expr.Variable "c") ) ]
    in
    assert_equal expected result ~printer:expr_list_to_string ;
    let input = "do a in x then b" in
    let result = parse input in
    let expected =
      [Expr.Do (Expr.Variable "a", Match.Ident "x", Expr.Variable "b")]
    in
    assert_equal expected result ~printer:expr_list_to_string

  let tests =
    "test_expressions"
    >::: [ literal_test
         ; grouping_expression_test
         ; dot_expression_test
         ; function_application_expression_test
         ; function_expression_test
         ; not_expression_test
         ; negate_expression_test
         ; multiplicative_expression_test
         ; additive_expression_test
         ; relational_expression_test
         ; equality_expression_test
         ; and_expression_test
         ; xor_expression_test
         ; or_expression_test
         ; boolean_precedence_test
         ; flat_join_expression_test
         ; join_expression_test
         ; match_expression_test
         ; if_expression_test
         ; let_expression_test
         ; do_expression_test ]
end

module Test_declaration = struct
  let parse = Tego_parser.Parser.parse_declaration

  let decl_list_to_string ds =
    List.map ds ~f:(fun d -> Decl.sexp_of_t d |> Sexp.to_string_hum)
    |> fun s -> "(" ^ String.concat ~sep:"; " s ^ ")"

  let expression_declaration_test =
    "expression_declaration_test"
    >:: fun _ ->
    let input = "def a = 1" in
    let result = parse input in
    let expected = [Decl.Expression ("a", Expr.Literal (Expr.Int 1))] in
    assert_equal expected result ~printer:decl_list_to_string ;
    let input = "def f x = x" in
    let result = parse input in
    let expected =
      [Decl.Expression ("f", Expr.Fn (Match.Ident "x", Expr.Variable "x"))]
    in
    assert_equal expected result ~printer:decl_list_to_string ;
    let input = "def f x y = x y" in
    let result = parse input in
    let expected =
      [ Decl.Expression
          ( "f"
          , Expr.Fn
              ( Match.Ident "x"
              , Expr.Fn
                  ( Match.Ident "y"
                  , Expr.FnApp (Expr.Variable "x", Expr.Variable "y") ) ) ) ]
    in
    assert_equal expected result ~printer:decl_list_to_string

  let tests = "test_declaration" >::: [expression_declaration_test]
end

module Test_program = struct
  let parse = Tego_parser.Parser.parse_program

  let program_list_to_string ps =
    List.map ps ~f:(fun p -> Prog.sexp_of_t p |> Sexp.to_string_hum)
    |> fun s -> "(" ^ String.concat ~sep:"; " s ^ ")"

  let program_test =
    "program_test"
    >:: fun _ ->
    let input = "def a = 1" in
    let result = parse input in
    let expected = [[Decl.Expression ("a", Expr.Literal (Expr.Int 1))]] in
    assert_equal expected result ~printer:program_list_to_string ;
    let input = "def a = 1 \n def b = 2" in
    let result = parse input in
    let expected =
      [ [ Decl.Expression ("a", Expr.Literal (Expr.Int 1))
        ; Decl.Expression ("b", Expr.Literal (Expr.Int 2)) ] ]
    in
    assert_equal expected result ~printer:program_list_to_string ;
    let input = "def a = 1 \n def b = 2 \n def c = 3" in
    let result = parse input in
    let expected =
      [ [ Decl.Expression ("a", Expr.Literal (Expr.Int 1))
        ; Decl.Expression ("b", Expr.Literal (Expr.Int 2))
        ; Decl.Expression ("c", Expr.Literal (Expr.Int 3)) ] ]
    in
    assert_equal expected result ~printer:program_list_to_string

  let tests = "test_program" >::: [program_test]
end

let () =
  run_test_tt_main
    ( "test_tego_parser"
    >::: [ Test_match.tests
         ; Test_expressions.tests
         ; Test_declaration.tests
         ; Test_program.tests ] )
