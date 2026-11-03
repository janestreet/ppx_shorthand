open! Core

module _ : sig
  type t [@@deriving compare]

  [%%rederive.nonportable: type nonrec t = t [@@deriving equal]]
end = struct
  type t = int

  let compare x y = x - y
  let equal x y = x = y
end

module _ : sig
  type t [@@deriving compare]

  [%%rederive.stateful: type nonrec t = t [@@deriving equal]]
end = struct
  type t =
    | A
    | B
  [@@warning "-unused-constructor"]

  let compare x y =
    match x, y with
    | A, A | B, B -> 0
    | A, B -> -1
    | B, A -> 1
  ;;

  let equal x y = Poly.equal x y
end
