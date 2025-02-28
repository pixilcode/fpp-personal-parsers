open! Core

module type S = sig
  type 'a parser

  val any_char : char parser

  val char : char -> char parser

  val char_where : (char -> bool) -> char parser

  val string : string -> string parser

  val string_where : len:int -> (string -> bool) -> string parser

  val take_while : (char -> bool) -> string parser
end

module Make (P : Parser_base.S with type Idx.token = char) :
  S with type 'a parser = 'a P.parser = struct
  (* TODO: write tests for these *)
  open P

  type 'a parser = 'a P.parser

  let any_char : char parser =
   fun (idx, callback) ->
    match Idx.token_at idx with
    | Some c ->
        callback (c, Idx.next idx)
    | None ->
        no_state_change

  let char (c : char) : char parser =
   fun (idx, callback) ->
    match Idx.token_at idx with
    | Some c' when Char.equal c c' ->
        callback (c, Idx.next idx)
    | _ ->
        no_state_change

  let char_where (f : char -> bool) : char parser =
   fun (idx, callback) ->
    match Idx.token_at idx with
    | Some c when f c ->
        callback (c, Idx.next idx)
    | _ ->
        no_state_change

  let string_where ~(len : int) (f : string -> bool) : string parser =
   fun (idx, callback) ->
    match Idx.tokens_at idx ~length:len with
    | Some s ->
        let s = String.of_char_list s in
        if f s then callback (s, Idx.next_nth idx len) else no_state_change
    | _ ->
        no_state_change

  let string (s : string) : string parser =
    let s_list = String.to_list s in
    fun (idx, callback) ->
      match Idx.tokens_at idx ~length:(String.length s) with
      | Some s' when List.equal Char.equal s_list s' ->
          callback (s, Idx.next_nth idx (String.length s))
      | _ ->
          no_state_change

  let take_while (f : char -> bool) : string parser =
   fun (idx, callback) ->
    let rec loop idx acc =
      match Idx.token_at idx with
      | Some c when f c ->
          loop (Idx.next idx) (c :: acc)
      | _ ->
          callback (String.of_char_list (List.rev acc), idx)
    in
    loop idx []
end
