open! Core

module Basic_idx :
  Parser_base.Idx with type source = string and type token = char = struct
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

module Make (C : Parser_base.CacheValue) = struct
  module Parser_base = Parser_base.Make (C) (Basic_idx)
  module Combinators = Combinators.Make (Parser_base)
  module Strings = Strings.Make (Parser_base)
  include Parser_base
  include Combinators
  include Strings
end
