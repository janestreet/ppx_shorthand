open! Core

[@@@disable_unused_warnings]

module T : sig
  val f : 'a list @ local -> ('a, string) Result.t @ local
  val g : int -> 'a list @ local -> int * 'a list @ local
end = struct
  let f =
    function%exclave
    | [] -> Error "empty"
    | hd :: _ -> Ok hd
  ;;

  let rec g x =
    function%exclave
    | [] -> x, []
    | hd :: tl ->
      let x, tl = g (x + 1) tl in
      x, hd :: tl
  ;;
end

module Unreachable : sig
  type t =
    | A : Nothing.t -> t
    | B : t

  val get : t -> unit option @ local
end = struct
  type t =
    | A : Nothing.t -> t
    | B : t

  let get =
    function%exclave
    | A _ -> .
    | B -> Some ()
  ;;
end
