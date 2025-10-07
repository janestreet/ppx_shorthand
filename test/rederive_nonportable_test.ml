open! Core

module _ : sig @@ portable
  type t [@@deriving compare]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal]]
end = struct
  type t = int

  let (compare @ portable) x y = x - y
  let (equal @ nonportable) x y = x = y
end
