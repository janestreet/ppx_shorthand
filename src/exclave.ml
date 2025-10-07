open! Stdppx
open! Import

let extension =
  Extension.declare
    "shorthand.exclave"
    Extension.Context.expression
    Ast_pattern.(pstr (pstr_eval (pexp_function __) nil ^:: nil))
    (fun ~loc ~path:(_ : string) cases ->
      let open Ast_builder in
      let loc = { loc with loc_ghost = true } in
      pexp_function
        ~loc
        (List.map cases ~f:(fun case ->
           match case.pc_rhs.pexp_desc with
           | Pexp_unreachable -> case
           | _ -> { case with pc_rhs = [%expr exclave_ [%e case.pc_rhs]] })))
;;

let extensions = [ extension ]
