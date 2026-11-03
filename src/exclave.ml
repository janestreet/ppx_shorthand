open! Stdppx
open! Import

let extension =
  Extension.declare
    "shorthand.exclave"
    Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun ~loc ~path:(_ : string) expr ->
      (* Match a [function] expression by hand rather than with
         [Ast_pattern.pexp_function], whose type differs between our internal ppxlib and
         upstream ppxlib. *)
      Ppxlib.assert_no_attributes expr.pexp_attributes;
      match
        Ppxlib_jane.Shim.Pexp_function.of_parsetree expr.pexp_desc ~loc:expr.pexp_loc
      with
      | Some ([], _, Pfunction_cases (cases, _, _)) ->
        let loc = { loc with loc_ghost = true } in
        Ast_builder.pexp_function_cases
          ~loc
          (List.map cases ~f:(fun case ->
             match case.pc_rhs.pexp_desc with
             | Pexp_unreachable -> case
             | _ -> { case with pc_rhs = [%expr [%e case.pc_rhs]] }))
      | None | Some (_ :: _, _, _) | Some ([], _, Pfunction_body _) ->
        Location.raise_errorf ~loc:expr.pexp_loc "expected a [function] expression")
;;

let extensions = [ extension ]
