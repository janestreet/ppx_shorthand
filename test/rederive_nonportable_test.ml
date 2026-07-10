open! Core

module _ : sig @@ portable
  type t [@@deriving compare ~localize]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal ~localize]]
end = struct
  type t = int

  let%template[@mode m = (local, global)] (compare @ portable) x y = x - y
  let%template[@mode m = (local, global)] (equal @ nonportable) x y = x = y
end
