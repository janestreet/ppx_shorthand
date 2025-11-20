open! Core

(* Successful compilation proves that we have implemented an [Equal.S] module with the
   expected type exposed.

   Below, we implement each of these modules by first defining the type, and then using
   [%%erase] to implement [Equal.S]. This would fail to compile if the type was not erased
   via destructive substitution. *)
module Proof (M : T) (_ : Equal.S with type t = M.t) = struct end
module Proof1 (M : T1) (_ : Equal.S1 with type 'a t = 'a M.t) = struct end
module Proof2 (M : T2) (_ : Equal.S2 with type ('a, 'b) t = ('a, 'b) M.t) = struct end

module%test [@name "nullary type"] _ =
  Proof
    (Unit)
    ((
     struct
       type t = unit

       [%%rederive type t = unit [@@deriving equal]]
     end :
     sig
       type t = unit

       [%%rederive: type t = unit [@@deriving equal]]
     end))

module%test [@name "unary type"] _ =
  Proof1
    (Option)
    ((
     struct
       type 'a t = 'a option

       [%%rederive type 'a t = 'a option [@@deriving equal]]
     end :
     sig
       type 'a t = 'a option

       [%%rederive: type 'a t = 'a option [@@deriving equal]]
     end))

module%test [@name "binary type"] _ =
  Proof2
    (Either)
    ((
     struct
       type ('a, 'b) t = ('a, 'b) Either.t

       [%%rederive type ('a, 'b) t = ('a, 'b) Either.t [@@deriving equal]]
     end :
     sig
       type ('a, 'b) t = ('a, 'b) Either.t

       [%%rederive: type ('a, 'b) t = ('a, 'b) Either.t [@@deriving equal]]
     end))

module%test [@name "deriving_inline"] _ =
  Proof
    (Unit)
    ((
     struct
       type t = unit

       [%%rederive
       type t = unit [@@deriving_inline equal]

       let _ = fun (_ : t) -> ()
       let equal = (equal_unit : t -> t -> bool)
       let _ = equal

       [@@@end]]
     end :
     sig
       type t = unit

       [%%rederive:
       type t = unit [@@deriving_inline equal]

       include sig
         [@@@ocaml.warning "-32"]

         include Ppx_compare_lib.Equal.S with type t := t
       end
       [@@ocaml.doc "@inline"]

       [@@@end]]
     end))

module%test [@name "re-exported type"] _ =
  Proof1
    (Option)
    ((
     struct
       type 'a t = 'a option =
         | None
         | Some of 'a

       [%%rederive
         type nonrec 'a t = 'a t =
           | None
           | Some of 'a
         [@@deriving equal]]
     end :
     sig
       type 'a t = 'a option =
         | None
         | Some of 'a

       [%%rederive:
         type nonrec 'a t = 'a t =
           | None
           | Some of 'a
         [@@deriving equal]]
     end))

module%test [@name "re-exported private type"] _ : sig
  type 'a t = private
    | None
    | Some of 'a

  [%%rederive:
    type nonrec 'a t = 'a t = private
      | None
      | Some of 'a
    [@@deriving equal]]
end =
(* We don't use [Proof1] here because a private type can only be equal to itself, so there
   isn't a good module to pass as the first argument to the functor. *)
struct
  type 'a t = 'a option =
    | None
    | Some of 'a

  [%%rederive
    type nonrec 'a t = 'a t =
      | None
      | Some of 'a
    [@@deriving equal]]
end

module%test [@name "fully-qualified ppxlib.deriving"] _ =
  Proof
    (Unit)
    ((
     struct
       type t = unit

       [%%rederive type t = unit [@@ppxlib.deriving equal]]
     end :
     sig
       type t = unit

       [%%rederive: type t = unit [@@ppxlib.deriving equal]]
     end))

module%test [@name "fully-qualified ppxlib.deriving_inline"] _ =
  Proof
    (Unit)
    ((
     struct
       type t = unit

       [%%rederive
       type t = unit [@@ppxlib.deriving_inline equal]

       let _ = fun (_ : t) -> ()
       let equal = (equal_unit : t -> t -> bool)
       let _ = equal

       [@@@end]]
     end :
     sig
       type t = unit

       [%%rederive:
       type t = unit [@@ppxlib.deriving_inline equal]

       include sig
         [@@@ocaml.warning "-32"]

         include Ppx_compare_lib.Equal.S with type t := t
       end
       [@@ocaml.doc "@inline"]

       [@@@end]]
     end))

module%test [@name "preceding attribute"] _ = struct
  module T = struct
    type t = { t : unit } [@@unboxed]
  end

  include
    Proof
      (T)
      ((
       struct
         type t = T.t

         [%%rederive type nonrec t = T.t = { t : unit } [@@unboxed] [@@deriving equal]]
       end :
       sig
         type t = T.t

         [%%rederive: type nonrec t = T.t = { t : unit } [@@unboxed] [@@deriving equal]]
       end))
end

module%test [@name "following attribute"] _ = struct
  module T = struct
    type t = { t : unit } [@@unboxed]
  end

  include
    Proof
      (T)
      ((
       struct
         type t = T.t

         [%%rederive type nonrec t = T.t = { t : unit } [@@deriving equal] [@@unboxed]]
       end :
       sig
         type t = T.t

         [%%rederive: type nonrec t = T.t = { t : unit } [@@deriving equal] [@@unboxed]]
       end))
end

module%test [@name "constraint"] _ : sig
  type 'a t constraint 'a = [< `foo | `bar ] [@@deriving equal]
end =
(* We don't use [Proof1] here because we'd need to introduce a new functor with a
   constraint on the type variable of its first argument. *)
(
struct
  type 'a t = T of 'a constraint 'a = [< `foo | `bar ]

  [%%rederive
    type nonrec 'a t = 'a t = T of 'a constraint 'a = [< `foo | `bar ]
    [@@deriving equal]]
end :
sig
  type 'a t = T of 'a constraint 'a = [< `foo | `bar ]

  [%%rederive:
    type nonrec 'a t = 'a t = T of 'a constraint 'a = [< `foo | `bar ]
    [@@deriving equal]]
end)

module%test [@name "doc comments"] _ = struct
  module T = struct
    type t = unit
  end

  include
    Proof
      (T)
      ((
       struct
         type t = T.t

         [%%rederive
         (** Doc comment *)

         type t = unit [@@deriving equal]]
       end :
       sig
         type t = T.t

         [%%rederive:
         (** Doc comment *)

         type t = unit [@@deriving equal]]
       end))
end

module%test [@name "template"] _ : sig
  type%template t = unit [@@kind k = (value, bits64)] [@@deriving equal]
end = (
struct
  [%%template
  [@@@kind.default k = (value, bits64)]

  type t = unit

  [%%rederive type t = unit [@@kind k] [@@deriving equal]]]
end :
sig
  [%%template:
  [@@@kind.default k = (value, bits64)]

  type t = unit

  [%%rederive: type t = unit [@@kind k] [@@deriving equal]]]
end)
