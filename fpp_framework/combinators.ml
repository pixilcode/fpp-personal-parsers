open! Core

module Make (P : Parser_base.S) = struct
  open P

  let fail = nothing

  let map (parser : 'a parser) ~(f : 'a -> 'b) : 'b parser =
    sequence parser ~f:(fun v -> unit (f v))

  let preceded (parser1 : 'a parser) (parser2 : 'b parser) : 'b parser =
    sequence parser1 ~f:(fun _ -> parser2)

  let terminated (parser1 : 'a parser) (parser2 : 'b parser) : 'a parser =
    sequence parser1 ~f:(fun v -> preceded parser2 (unit v))

  let delimited (open_parser : 'a parser) (parser : 'b parser)
      (close_parser : 'c parser) : 'b parser =
    preceded open_parser (terminated parser close_parser)

  let opt (parser : 'a parser) : 'a option parser =
    choice (map parser ~f:(fun v -> Some v)) (unit None)

  let pair (parser1 : 'a parser) (parser2 : 'b parser) : ('a * 'b) parser =
    sequence parser1 ~f:(fun v1 ->
        sequence parser2 ~f:(fun v2 -> unit (v1, v2)) )

  let separated_pair (parser1 : 'a parser) (separator : 'b parser)
      (parser2 : 'c parser) : ('a * 'c) parser =
    pair (terminated parser1 separator) parser2

  let run_if ~(cond : bool) (parser : 'a parser) : 'a parser =
    if cond then parser else fail

  let run_if_else ~(cond : bool) (parser1 : 'a parser) (parser2 : 'a parser) :
      'a parser =
    if cond then parser1 else parser2

  let verify (parser : 'a parser) ~(f : 'a -> bool) : 'a parser =
    sequence parser ~f:(fun v -> if f v then unit v else fail)

  let any_of (parsers : 'a parser list) : 'a parser =
    List.fold parsers ~init:nothing ~f:(fun acc p -> choice acc p)

  let sequence_opt (parser : 'a option parser) ~(f : 'a -> 'b parser) :
      'b option parser =
    sequence parser ~f:(fun v ->
        match v with Some v -> map (f v) ~f:Option.some | None -> unit None )

  module Infix_ops = struct
    let ( >>= ) parser f = sequence parser ~f

    let ( >>| ) parser f = map parser ~f

    let ( >>* ) = preceded

    let ( *>> ) = terminated

    let ( <|> ) = choice

    let ( <&> ) = pair
  end
end
