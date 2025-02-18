open! Core

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

module Test_base = struct
  open OUnit2

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
  open OUnit2

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

  let tests =
    "test_combinators"
    >::: [ fail_test
         ; map_test
         ; preceded_test
         ; terminated_test
         ; delimited_test
         ; opt_test ]
end

module Test_strings = struct
  let tests = OUnit2.("test_strings" >::: [])
end

let () =
  OUnit2.run_test_tt_main
    OUnit2.(
      "test_fpp_framework"
      >::: [Test_base.tests; Test_combinators.tests; Test_strings.tests] )
