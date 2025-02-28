open! Core
open OUnit2

(* NOTE: tests MUST consume the full string, or else the test will return an
         empty list *)

module Test_idx :
  Fpp.Parser_base.Idx with type source = string and type token = char = struct
  module T = struct
    type t = int * string

    type source = string

    type token = char

    let create_from_source source = (0, source)

    let next (idx, source) = (idx + 1, source)

    let next_nth (idx, source) n = (idx + n, source)

    let is_at_end (idx, source) = idx >= String.length source

    let token_at (idx, source) =
      if is_at_end (idx, source) then None else Some source.[idx]

    let tokens_at (idx, source) ~length =
      if is_at_end (idx, source) then None
      else
        let end_idx = idx + length in
        if end_idx > String.length source then None
        else
          String.sub source ~pos:idx ~len:length
          |> String.to_list |> Option.some

    let compare (idx1, source1) (idx2, source2) =
      match Int.compare idx1 idx2 with
      | 0 ->
          String.compare source1 source2
      | c ->
          c

    let sexp_of_t (idx, source) =
      Sexp.List [Int.sexp_of_t idx; String.sexp_of_t source]

    let t_of_sexp sexp =
      match sexp with
      | Sexp.List [idx; source] ->
          (Int.t_of_sexp idx, String.t_of_sexp source)
      | _ ->
          failwith "Invalid index"

    let hash = Hashtbl.hash

    let hash_fold_t state (idx, source) =
      let state = Int.hash_fold_t state idx in
      String.hash_fold_t state source
  end

  include T
  include Hashable.Make (T)
end

module Test_cache_value : Fpp.Parser_base.CacheValue = String

module Parser_base :
  Fpp.Parser_base.S
    with module Idx = Test_idx
     and type cached_value = Test_cache_value.t =
  Fpp.Parser_base.Make (Test_cache_value) (Test_idx)

module Combinators = Fpp.Combinators.Make (Parser_base)
module Strings = Fpp.Strings.Make (Parser_base)

let int_list_printer = List.to_string ~f:Int.to_string

let opt_int_list_printer =
  List.to_string ~f:(Option.value_map ~default:"None" ~f:Int.to_string)

let int_pair_list_printer =
  List.to_string ~f:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b)

let unit_list_printer = List.to_string ~f:(fun () -> "()")

let char_list_printer = List.to_string ~f:Char.to_string

let char_list_list_printer =
  List.to_string ~f:(List.to_string ~f:Char.to_string)

let string_list_printer = List.to_string ~f:Fn.id

module Test_base = struct
  let nothing_test =
    "nothing"
    >:: fun _ ->
    let result = Parser_base.run Parser_base.nothing "" in
    assert_equal [] result ~printer:int_list_printer

  let unit_test =
    "unit"
    >:: fun _ ->
    let parser = Parser_base.unit 1 in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer

  let sequence_test =
    "sequence"
    >:: fun _ ->
    let parser =
      Parser_base.sequence (Parser_base.unit 1) ~f:(fun v ->
          Parser_base.unit (v + 1) )
    in
    let result = Parser_base.run parser "" in
    assert_equal [2] result ~printer:int_list_printer

  let choice_test =
    "choice"
    >:: fun _ ->
    let parser = Parser_base.choice (Parser_base.unit 1) (Parser_base.unit 2) in
    let result = Parser_base.run parser "" in
    assert_equal [2; 1] result ~printer:int_list_printer

  let tests =
    "test_base" >::: [nothing_test; unit_test; sequence_test; choice_test]
end

module Test_combinators = struct
  let fail_test =
    "fail"
    >:: fun _ ->
    (* fail on a given string *)
    let parser = Combinators.fail in
    let result = Parser_base.run parser "test" in
    assert_equal [] result ~printer:int_list_printer ;
    (* fail on an empty string *)
    let result = Parser_base.run Combinators.fail "" in
    assert_equal [] result ~printer:int_list_printer

  let map_test =
    "map"
    >:: fun _ ->
    (* map a parser to a different value *)
    let parser = Combinators.map (Parser_base.unit 1) ~f:(fun v -> v + 1) in
    let result = Parser_base.run parser "" in
    assert_equal [2] result ~printer:int_list_printer

  let preceded_test =
    "preceded"
    >:: fun _ ->
    (* parse two parsers in sequence and return the result of the second *)
    let parser =
      Combinators.preceded (Parser_base.unit "ignored") (Parser_base.unit 1)
    in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer

  let terminated_test =
    "terminated"
    >:: fun _ ->
    (* parse two parsers in sequence and return the result of the first *)
    let parser =
      Combinators.terminated (Parser_base.unit 1) (Parser_base.unit "ignored")
    in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer

  let delimited_test =
    "delimited"
    >:: fun _ ->
    (* parse three parsers in sequence and return the result of the second *)
    let parser =
      Combinators.delimited
        (Parser_base.unit "ignored")
        (Parser_base.unit 1)
        (Parser_base.unit "ignored")
    in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer

  let opt_test =
    "opt"
    >:: fun _ ->
    (* parse a parser and return Some value if it succeeds *)
    let parser = Combinators.opt (Parser_base.unit 1) in
    let result = Parser_base.run parser "" in
    assert_equal [None; Some 1] result ~printer:opt_int_list_printer ;
    (* parse a parser and return None if it fails *)
    let parser = Combinators.opt Combinators.fail in
    let result = Parser_base.run parser "" in
    assert_equal [None] result ~printer:opt_int_list_printer

  let pair_test =
    "pair"
    >:: fun _ ->
    (* parse two parsers in sequence and return a tuple of the results *)
    let parser = Combinators.pair (Parser_base.unit 1) (Parser_base.unit 2) in
    let result = Parser_base.run parser "" in
    assert_equal [(1, 2)] result ~printer:int_pair_list_printer

  let separated_pair_test =
    "separated_pair"
    >:: fun _ ->
    (* parse three parsers in sequence and return a tuple of the first and third *)
    let parser =
      Combinators.separated_pair (Parser_base.unit 1)
        (Parser_base.unit "ignored")
        (Parser_base.unit 2)
    in
    let result = Parser_base.run parser "" in
    assert_equal [(1, 2)] result ~printer:int_pair_list_printer

  let parse_if_else_test =
    "parse_if_else"
    >:: fun _ ->
    (* run a parser if a condition is true, otherwise run another parser *)
    let parser =
      Combinators.parse_if_else ~cond:true (Parser_base.unit 1)
        (Parser_base.unit 2)
    in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer ;
    (* run a parser if a condition is false, otherwise run another parser *)
    let parser =
      Combinators.parse_if_else ~cond:false (Parser_base.unit 1)
        (Parser_base.unit 2)
    in
    let result = Parser_base.run parser "" in
    assert_equal [2] result ~printer:int_list_printer

  let parse_if_test =
    "parse_if"
    >:: fun _ ->
    (* run a parser if a condition is true *)
    let parser = Combinators.parse_if ~cond:true (Parser_base.unit 1) in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer ;
    (* don't run a parser if a condition is false *)
    let parser = Combinators.parse_if ~cond:false (Parser_base.unit 1) in
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:int_list_printer

  let verify_test =
    "verify"
    >:: fun _ ->
    (* run a parser and return the result if it satisfies a predicate *)
    let parser = Combinators.verify (Parser_base.unit 1) ~f:(fun v -> v = 1) in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer ;
    (* don't return a parser result if it doesn't satisfy a predicate *)
    let parser = Combinators.verify (Parser_base.unit 1) ~f:(fun v -> v = 2) in
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:int_list_printer

  let any_of_test =
    "any_of"
    >:: fun _ ->
    (* try all of the parsers *)
    let parser =
      Combinators.any_of
        [Parser_base.unit 1; Parser_base.unit 2; Parser_base.unit 3]
    in
    let result = Parser_base.run parser "" in
    assert_equal [3; 2; 1] result ~printer:int_list_printer ;
    (* ignore the parsers that fail *)
    let parser = Combinators.any_of [Parser_base.unit 1; Parser_base.nothing] in
    let result = Parser_base.run parser "" in
    assert_equal [1] result ~printer:int_list_printer

  let sequence_opt_test =
    "sequence_opt"
    >:: fun _ ->
    (* run a parser and return Some value if it succeeds *)
    let parser =
      Combinators.sequence_opt (Parser_base.unit (Some 1)) ~f:(fun v ->
          Parser_base.unit (v + 1) )
    in
    let result = Parser_base.run parser "" in
    assert_equal [Some 2] result ~printer:opt_int_list_printer ;
    (* run a parser and return None if it fails *)
    let parser =
      Combinators.sequence_opt (Parser_base.unit None) ~f:(fun v ->
          Parser_base.unit (v + 1) )
    in
    let result = Parser_base.run parser "" in
    assert_equal [None] result ~printer:opt_int_list_printer

  let end_of_input_test =
    "end_of_input"
    >:: fun _ ->
    (* succeed if the input is empty *)
    let parser = Combinators.end_of_input in
    let result = Parser_base.run parser "" in
    assert_equal [()] result ~printer:unit_list_printer ;
    (* fail if the input is not empty *)
    let result = Parser_base.run parser "test" in
    assert_equal [] result ~printer:unit_list_printer

  let skip_forward_test =
    "skip_forward"
    >:: fun _ ->
    (* skip to the end of the input *)
    let parser =
      Combinators.preceded (Combinators.skip_forward 4) Combinators.end_of_input
    in
    let result = Parser_base.run parser "test" in
    assert_equal [()] result ~printer:unit_list_printer ;
    (* succeed even if it goes past the end *)
    let result = Parser_base.run parser "" in
    assert_equal [()] result ~printer:unit_list_printer ;
    (* fail if the input is too long *)
    let result = Parser_base.run parser "test is" in
    assert_equal [] result ~printer:unit_list_printer

  let many_test =
    "many"
    >:: fun _ ->
    (* parse a parser zero or more times *)
    let parser = Combinators.many Strings.any_char in
    let result = Parser_base.run parser "" in
    assert_equal [[]] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "1" in
    assert_equal [['1']] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "11" in
    assert_equal [['1'; '1']] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "111" in
    assert_equal [['1'; '1'; '1']] result ~printer:char_list_list_printer

  let many1_test =
    "many1"
    >:: fun _ ->
    (* parse a parser one or more times *)
    let parser = Combinators.many1 Strings.any_char in
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "1" in
    assert_equal [['1']] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "11" in
    assert_equal [['1'; '1']] result ~printer:char_list_list_printer ;
    let result = Parser_base.run parser "111" in
    assert_equal [['1'; '1'; '1']] result ~printer:char_list_list_printer

  let peek_test =
    "peek"
    >:: fun _ ->
    (* peek at the next character *)
    let parser =
      Combinators.terminated
        (Combinators.peek (Strings.char '1'))
        Strings.any_char
    in
    let result = Parser_base.run parser "1" in
    assert_equal ['1'] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "2" in
    assert_equal [] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:char_list_printer

  let tests =
    "test_combinators"
    >::: [ fail_test
         ; map_test
         ; preceded_test
         ; terminated_test
         ; delimited_test
         ; opt_test
         ; pair_test
         ; separated_pair_test
         ; parse_if_else_test
         ; parse_if_test
         ; verify_test
         ; any_of_test
         ; sequence_opt_test
         ; end_of_input_test
         ; skip_forward_test
         ; many_test
         ; many1_test
         ; peek_test ]
end

module Test_strings = struct
  let any_char_test =
    "any_char"
    >:: fun _ ->
    let parser = Strings.any_char in
    let result = Parser_base.run parser "1" in
    assert_equal ['1'] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:char_list_printer

  let char_test =
    "char"
    >:: fun _ ->
    let parser = Strings.char '1' in
    let result = Parser_base.run parser "1" in
    assert_equal ['1'] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "2" in
    assert_equal [] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:char_list_printer

  let char_where_test =
    "char_where"
    >:: fun _ ->
    let parser = Strings.char_where (fun c -> Char.equal c '1') in
    let result = Parser_base.run parser "1" in
    assert_equal ['1'] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "2" in
    assert_equal [] result ~printer:char_list_printer ;
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:char_list_printer

  let string_test =
    "string"
    >:: fun _ ->
    let parser = Strings.string "test" in
    let result = Parser_base.run parser "test" in
    assert_equal ["test"] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "toast" in
    assert_equal [] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "" in
    assert_equal [] result ~printer:string_list_printer

  let take_while_test =
    "take_while"
    >:: fun _ ->
    let parser = Strings.take_while (fun c -> Char.equal c '1') in
    let result = Parser_base.run parser "" in
    assert_equal [""] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "1" in
    assert_equal ["1"] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "11" in
    assert_equal ["11"] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "111" in
    assert_equal ["111"] result ~printer:string_list_printer ;
    let result = Parser_base.run parser "211" in
    assert_equal [] result ~printer:string_list_printer

  let tests =
    "test_strings"
    >::: [ any_char_test
         ; char_test
         ; char_where_test
         ; string_test
         ; take_while_test ]
end

let () =
  run_test_tt_main
    ( "test_fpp_framework"
    >::: [Test_base.tests; Test_combinators.tests; Test_strings.tests] )
