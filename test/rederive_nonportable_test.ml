open! Core

module _ : sig
  type t [@@deriving compare ~localize]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal ~localize]]
end = struct
  type t = int

  let%template[@mode m = (local, global)] compare x y = x - y
  let%template[@mode m = (local, global)] equal x y = x = y
end
