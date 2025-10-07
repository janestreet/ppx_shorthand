open! Core

let%expect_test "simple argument reordering (constraint)" =
  let f : char -> y:char -> ?z:char -> unit -> string =
    fun x ~y ?(z = 'z') () -> String.of_array [| x; y; z |]
  in
  let g : ?z:char -> char -> y:char -> unit -> string = [%eta (f : ?z:_ -> _)] in
  let h : y:char -> ?z:char -> char -> unit -> string = [%eta (g : y:_ -> _)] in
  let i : char -> y:char -> ?z:char -> unit -> string =
    [%eta (h : _ -> y:_ -> ?z:_ -> _)]
  in
  print_endline (f 'a' ~y:'b' ~z:'c' ());
  [%expect {| abc |}];
  print_endline (g ~z:'c' 'a' ~y:'b' ());
  [%expect {| abc |}];
  print_endline (h ~y:'b' ~z:'c' 'a' ());
  [%expect {| abc |}];
  print_endline (i 'a' ~y:'b' ~z:'c' ());
  [%expect {| abc |}]
;;

let%test_unit "partial application w/ or w/o optional argument" =
  let f : ?a:unit -> unit -> unit -> unit = fun ?(a = ()) () () -> a in
  (* w/ optional argument *)
  let g : ?a:unit -> unit -> unit = [%eta (f () : ?a:_ -> _)] in
  g ~a:() ();
  (* w/o optional argument *)
  let h : unit -> unit = [%eta (f () : _ -> _)] in
  h ()
;;

let%expect_test "eta-expand binary function of local arguments" =
  (* test as [external] *)
  let open struct
    external f : float @ local -> float @ local -> float = "%addfloat"
  end in
  let g : float -> float -> float = [%eta (f : _ -> _ -> _)] in
  print_s [%sexp (g 5. 10. : float)];
  [%expect {| 15 |}];
  (* test as [let] *)
  let h : float @ local -> float @ local -> float = fun x y -> f x y in
  let i : float -> float -> float = [%eta (h : _ -> _ -> _)] in
  print_s [%sexp (i 10. 5. : float)];
  [%expect {| 15 |}]
;;

let%expect_test "local-returning" =
  (* test as [external] *)
  let open struct
    external f : float @ local -> float @ local -> float @ local = "%addfloat"
  end in
  let g : float -> float -> float @ local = [%eta (f : _ -> _ -> _ @ local)] in
  print_s [%sexp (globalize_float (g 5. 10.) : float)];
  [%expect {| 15 |}];
  (* test as [let] *)
  let h : float @ local -> float @ local -> float @ local = fun x y -> exclave_ f x y in
  let i : float -> float -> float @ local = [%eta (h : _ -> _ -> _ @ local)] in
  print_s [%sexp (globalize_float (i 10. 5.) : float)];
  [%expect {| 15 |}]
;;

(* The fact that this compiles given the [@inline] attributes on each definition proves
   the PPX is working as intended, since [@inline] may only appear on syntactic functions
   and not values. *)
open struct
  let int_abs = [%eta1 Int.abs]
  let int_equal = [%eta2 Int.equal]
  let array_set = [%eta3 Array.set]
  let%template option_value_local_exn = [%eta1.exclave Option.value_exn [@mode local]]
  let%template option_first_some_local = [%eta2.exclave Option.first_some [@mode local]]
  let bool_select = [%eta3.exclave Bool.select]
end

let%expect_test "etaN for eta{1,2,3}" =
  let () =
    print_s [%sexp (int_abs (-4) : int)];
    [%expect {| 4 |}]
  in
  let () =
    print_s [%sexp (int_equal 0 0 : bool)];
    [%expect {| true |}]
  in
  let () =
    let array = [| 'X' |] in
    array_set array 0 'Y';
    print_s [%sexp (array.(0) : char)];
    [%expect {| Y |}]
  in
  ()
;;

let%expect_test "etaN.exclave for eta{1,2,3}" =
  let () =
    let some = stack_ Some 3 in
    print_s [%sexp (option_value_local_exn some : int)];
    [%expect {| 3 |}]
  in
  let () =
    let some = stack_ Some 10 in
    print_s [%sexp (option_value_local_exn (option_first_some_local None some) : int)];
    [%expect {| 10 |}]
  in
  let () =
    let some = stack_ Some 13 in
    print_s [%sexp (option_value_local_exn (bool_select false None some) : int)];
    [%expect {| 13 |}]
  in
  ()
;;

(* Check that the [@inline] attribute is present. *)
let f () () = ()

[@@@ocamlformat "disable"]
[@@@expand_inline let _ = [%eta (f : _ -> _)]]

let _ = ((fun __eta_0 -> (f (__eta_0 : _) : _))[@inline ])

[@@@end]
[@@@ocamlformat "enable"]
