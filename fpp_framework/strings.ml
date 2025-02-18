open! Core

module type Idx_string = sig
  include Parser_base.Idx

  val char_at : t -> char option

  val string_at : t -> length:int -> string option
end

module type S = sig
  type 'a parser

  type token

  val any_token : char parser

  val char : char -> char parser

  val char_where : (char -> bool) -> char parser

  val string : string -> string parser

  val take_while : (char -> bool) -> string parser
end

module Make (P : sig
  module Idx : Idx_string

  include Parser_base.S with module Idx := Idx
end) =
struct
  (* TODO: write tests for these *)
  open P

  type 'a parser = 'a P.parser

  let any_char : char parser =
   fun (idx, callback) ->
    match Idx.char_at idx with
    | Some c ->
        callback (c, Idx.next idx)
    | None ->
        no_state_change

  let char (c : char) : char parser =
   fun (idx, callback) ->
    match Idx.char_at idx with
    | Some c' when Char.equal c c' ->
        callback (c, Idx.next idx)
    | _ ->
        no_state_change

  let char_where (f : char -> bool) : char parser =
   fun (idx, callback) ->
    match Idx.char_at idx with
    | Some c when f c ->
        callback (c, Idx.next idx)
    | _ ->
        no_state_change

  let string (s : string) : string parser =
   fun (idx, callback) ->
    match Idx.string_at idx ~length:(String.length s) with
    | Some s' when String.equal s s' ->
        callback (s, Idx.next_nth idx (String.length s))
    | _ ->
        no_state_change

  let take_while (f : char -> bool) : string parser =
   fun (idx, callback) ->
    let rec loop idx acc =
      match Idx.char_at idx with
      | Some c when f c ->
          loop (Idx.next idx) (c :: acc)
      | _ ->
          callback (String.of_char_list (List.rev acc), idx)
    in
    loop idx []
end
