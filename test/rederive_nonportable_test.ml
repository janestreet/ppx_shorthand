open! Core

module _ : sig @@ portable
  type t [@@deriving compare ~localize]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal ~localize]]
end = struct
  type t = int

  let%template[@mode m = (local, global)] (compare @ portable) x y = x - y
  let%template[@mode m = (local, global)] (equal @ nonportable) x y = x = y
end

module _ : sig @@ stateless
  type t [@@deriving compare ~localize]

  [%%rederive.stateful: type nonrec t = t [@@deriving equal ~localize]]
end = struct
  type t =
    | A
    | B
  [@@warning "-unused-constructor"]

  let%template[@mode m = (local, global)] (compare @ stateless) x y =
    match x, y with
    | A, A | B, B -> 0
    | A, B -> -1
    | B, A -> 1
  ;;

  let%template[@mode m = (local, global)] (equal @ stateful) x y = Poly.equal x y
end
