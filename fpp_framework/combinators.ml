open! Core

module type S = sig
  type 'a parser

  val fail : 'a parser

  val map : 'a parser -> f:('a -> 'b) -> 'b parser

  val preceded : 'a parser -> 'b parser -> 'b parser

  val terminated : 'a parser -> 'b parser -> 'a parser

  val delimited : 'a parser -> 'b parser -> 'c parser -> 'b parser

  val opt : 'a parser -> 'a option parser

  val pair : 'a parser -> 'b parser -> ('a * 'b) parser

  val separated_pair : 'a parser -> 'b parser -> 'c parser -> ('a * 'c) parser

  val run_if_else : cond:bool -> 'a parser -> 'a parser -> 'a parser

  val run_if : cond:bool -> 'a parser -> 'a parser

  val verify : 'a parser -> f:('a -> bool) -> 'a parser

  val any_of : 'a parser list -> 'a parser

  val sequence_opt : 'a option parser -> f:('a -> 'b parser) -> 'b option parser

  val end_of_input : unit parser

  val skip_forward : int -> unit parser

  val many : 'a parser -> 'a list parser

  module Infix_ops : sig
    val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser

    val ( >>| ) : 'a parser -> ('a -> 'b) -> 'b parser

    val ( >>* ) : 'a parser -> 'b parser -> 'b parser

    val ( *>> ) : 'a parser -> 'b parser -> 'a parser

    val ( <|> ) : 'a parser -> 'a parser -> 'a parser

    val ( <&> ) : 'a parser -> 'b parser -> ('a * 'b) parser
  end
end

module Make (P : Parser_base.S) : S with type 'a parser = 'a P.parser = struct
  open P

  type 'a parser = 'a P.parser

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

  let run_if_else ~(cond : bool) (parser1 : 'a parser) (parser2 : 'a parser) :
      'a parser =
    if cond then parser1 else parser2

  let run_if ~(cond : bool) (parser : 'a parser) : 'a parser =
    run_if_else ~cond parser fail

  let verify (parser : 'a parser) ~(f : 'a -> bool) : 'a parser =
    sequence parser ~f:(fun v -> if f v then unit v else fail)

  let any_of (parsers : 'a parser list) : 'a parser =
    List.fold parsers ~init:nothing ~f:(fun acc p -> choice acc p)

  let sequence_opt (parser : 'a option parser) ~(f : 'a -> 'b parser) :
      'b option parser =
    sequence parser ~f:(fun v ->
        match v with Some v -> map (f v) ~f:Option.some | None -> unit None )

  let end_of_input : unit parser =
   fun (idx, callback) ->
    if Idx.is_at_end idx then callback ((), idx) else no_state_change

  let skip_forward (n : int) : unit parser =
   fun (idx, callback) -> callback ((), Idx.next_nth idx n)

  let many (parser : 'a parser) : 'a list parser =
    let rec many' (acc : 'a list) : 'a list parser =
      choice (sequence parser ~f:(fun v -> many' (v :: acc))) (unit acc)
    in
    map (many' []) ~f:List.rev

  module Infix_ops = struct
    let ( >>= ) parser f = sequence parser ~f

    let ( >>| ) parser f = map parser ~f

    let ( >>* ) = preceded

    let ( *>> ) = terminated

    let ( <|> ) = choice

    let ( <&> ) = pair
  end
end
