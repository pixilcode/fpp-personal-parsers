module type CacheValue = sig
  type t
end

module type Idx = sig
  type t
end

module M (Idx : Idx) (CacheValue : CacheValue) = struct
  type tag = string

  type parser_position = tag * Idx.t
end