open! Core

module _ : sig
  type t [@@deriving compare]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal]]
end = struct
  type t = int

  let compare x y = x - y
  let equal x y = x = y
end
