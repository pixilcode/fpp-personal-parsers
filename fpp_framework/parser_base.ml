open! Core

module type CacheValue = sig
  type t

  include Set.Elt with type t := t
end

module type Idx = sig
  type t

  (** [source] is the type that this index is built from. The most basic
      source type would be a [string]. However, this abstraction allows indices
      to be built from an input channel, a collection of bytes, or even an array
      of tokens. *)
  type source

  include Set.Elt with type t := t

  include Hashable.S with type t := t

  val next : t -> t

  val next_nth : t -> int -> t

  val is_at_end : t -> bool

  val create_from_source : source -> t
end

module type S = sig
  (* types *)
  module Idx : Idx

  type cached_value

  type tag = string

  type parser_position = tag * Idx.t

  type 'a parser_output = 'a * Idx.t

  type 'a parser_callback = 'a parser_output -> state_transformer

  and state_transformer

  type 'a parser = Idx.t * 'a parser_callback -> state_transformer

  (* basic parser combinators *)
  val nothing : 'a parser

  val unit : 'a -> 'a parser

  val sequence : 'a parser -> f:('a -> 'b parser) -> 'b parser

  val choice : 'a parser -> 'a parser -> 'a parser

  (* fixed-point parsing functions *)
  val no_state_change : state_transformer

  val memo : tag:tag -> cached_value parser -> cached_value parser

  val run : 'a parser -> Idx.source -> 'a list

  val run_raw :
       'a parser
    -> Idx.source
    -> (parser_position, cached_value parser_output list) Hashtbl.t
end

module Make (C : CacheValue) (I : Idx) :
  S with module Idx := I and type cached_value := C.t = struct
  (* types *)
  module Idx = I

  type cached_value = C.t

  type tag = string

  type parser_position = tag * Idx.t

  module Parser_position = struct
    type t = parser_position

    let compare (t1, i1) (t2, i2) =
      match String.compare t1 t2 with 0 -> I.compare i1 i2 | c -> c

    let sexp_of_t (tag, idx) = Sexp.List [Sexp.Atom tag; I.sexp_of_t idx]

    let hash (tag, idx) = Hashtbl.hash (tag, I.hash idx)
  end

  type 'a parser_output = 'a * Idx.t

  module CachedParserOutput = struct
    type t = cached_value parser_output

    let compare (value1, idx1) (value2, idx2) =
      match C.compare value1 value2 with 0 -> I.compare idx1 idx2 | c -> c

    let t_of_sexp sexp =
      let open Sexp in
      match sexp with
      | List [value; idx] ->
          (C.t_of_sexp value, I.t_of_sexp idx)
      | _ ->
          failwith "Invalid cached parser output"

    let sexp_of_t (value, idx) = Sexp.List [C.sexp_of_t value; I.sexp_of_t idx]
  end

  module CachedParserOutputSet = Set.Make (CachedParserOutput)

  type 'a parser_callback = 'a parser_output -> state_transformer

  and state =
    | State of
        ( parser_position
        , CachedParserOutputSet.t * cached_value parser_callback list )
        Hashtbl.t

  and state_transformer = state -> state

  type 'a parser = Idx.t * 'a parser_callback -> state_transformer

  (* basic parser combinators *)
  let nothing : 'a parser = fun _ -> fun state -> state

  let unit (value : 'a) : 'a parser =
   fun (idx, callback) -> callback (value, idx)

  let sequence (parser : 'a parser) ~(f : 'a -> 'b parser) : 'b parser =
   fun (idx, callback) ->
    let next_callback =
     fun (value, idx) ->
      let next_parser = f value in
      next_parser (idx, callback)
    in
    parser (idx, next_callback)

  let choice (parser1 : 'a parser) (parser2 : 'a parser) : 'a parser =
   fun (idx, callback) ->
    fun state -> state |> parser1 (idx, callback) |> parser2 (idx, callback)

  (* fixed-point parsing functions *)
  let no_state_change : state_transformer = fun state -> state

  let memo ~(tag : tag) (parser : cached_value parser) : cached_value parser =
   fun (idx, callback) ->
    let loc : parser_position = (tag, idx) in
    fun state ->
      let (State state) = state in
      match Hashtbl.find state loc with
      | Some (parser_outputs, callbacks) ->
          let new_callbacks = callback :: callbacks in
          Hashtbl.set state ~key:loc ~data:(parser_outputs, new_callbacks) ;
          Set.fold parser_outputs ~init:(State state)
            ~f:(fun state parser_output ->
              let state_transformer = callback parser_output in
              state_transformer state )
      | None ->
          let new_callback : cached_value parser_callback =
           fun parser_output ->
            fun state ->
             let (State state) = state in
             match Hashtbl.find state loc with
             | Some (parser_outputs, callbacks) ->
                 if Set.mem parser_outputs parser_output then State state
                 else
                   let new_parser_outputs =
                     Set.add parser_outputs parser_output
                   in
                   Hashtbl.set state ~key:loc
                     ~data:(new_parser_outputs, callbacks) ;
                   List.fold_left callbacks ~init:(State state)
                     ~f:(fun state callback ->
                       let state_transformer = callback parser_output in
                       state_transformer state )
             | None ->
                 failwith "unreachable"
          in
          Hashtbl.set state ~key:loc
            ~data:(CachedParserOutputSet.empty, [callback]) ;
          parser (idx, new_callback) (State state)

  let run (parser : 'a parser) (input : Idx.source) : 'a list =
    let results = ref [] in
    let index = I.create_from_source input in
    let callback =
     fun (value, idx) ->
      (* If the index is at the end of the string, then it is a valid parse,
           so we add it to the results. *)
      if Idx.is_at_end idx then results := value :: !results ;
      (* Either way, leave the state unmodified *)
      fun state -> state
    in
    let initial_state = State (Hashtbl.create (module Parser_position)) in
    ignore (parser (index, callback) initial_state) ;
    !results

  let run_raw (parser : 'a parser) (input : Idx.source) :
      (parser_position, cached_value parser_output list) Hashtbl.t =
    let index = I.create_from_source input in
    let callback = fun (_value, _idx) -> fun state -> state in
    let initial_state = State (Hashtbl.create (module Parser_position)) in
    let (State final_state) = parser (index, callback) initial_state in
    final_state
    |> Hashtbl.map ~f:(fun (parser_outputs, _callbacks) ->
           Set.to_list parser_outputs )
end
