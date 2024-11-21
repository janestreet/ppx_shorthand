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
    external f : float -> float -> float = "%addfloat"
  end in
  let g : float -> float -> float = [%eta (f : _ -> _ -> _)] in
  print_s [%sexp (g 5. 10. : float)];
  [%expect {| 15 |}];
  (* test as [let] *)
  let h : float -> float -> float = fun x y -> f x y in
  let i : float -> float -> float = [%eta (h : _ -> _ -> _)] in
  print_s [%sexp (i 10. 5. : float)];
  [%expect {| 15 |}]
;;

let%expect_test "local-returning" =
  (* test as [external] *)
  let open struct
    external f : float -> float -> float = "%addfloat"
  end in
  let g : float -> float -> float = [%eta (f : _ -> _ -> _)] in
  print_s [%sexp (globalize_float (g 5. 10.) : float)];
  [%expect {| 15 |}];
  (* test as [let] *)
  let h : float -> float -> float = fun x y -> f x y in
  let i : float -> float -> float = [%eta (h : _ -> _ -> _)] in
  print_s [%sexp (globalize_float (i 10. 5.) : float)];
  [%expect {| 15 |}]
;;
