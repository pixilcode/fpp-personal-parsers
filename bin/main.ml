open! Core

let read_standard_input () : string = In_channel.input_all In_channel.stdin

let parse_expr (input : string) : unit =
  let result = Expr_parser.Parser.parse input in
  match result with
  | [] ->
      print_endline "invalid expression"
  | [x] ->
      printf "%d\n" x
  | _ ->
      failwith "unreachable, parser is not ambiguous"

let parse (parser : string) (input : string option) : unit =
  let input = match input with Some s -> s | None -> read_standard_input () in
  match parser with
  | "expr" ->
      parse_expr input
  | _ ->
      failwith "unknown parser"

let () =
  let open Cmdliner in
  let parser_t : string Term.t =
    let doc = "the parser to use" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PARSER" ~doc)
  in
  let input_t : string option Term.t =
    let doc = "the input to parse" in
    Arg.(value & pos 1 (some string) None & info [] ~docv:"INPUT" ~doc)
  in
  let parse_t = Term.(const parse $ parser_t $ input_t) in
  let cmd =
    let doc = "parse a string" in
    let info = Cmd.info "parse" ~version:"%%VERSION%%" ~doc in
    Cmd.v info parse_t
  in
  exit (Cmd.eval cmd)
