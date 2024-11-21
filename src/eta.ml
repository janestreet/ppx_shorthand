open! Base
open! Import

type arrow =
  { args : (arg_label * core_type) list
  ; ret : core_type
  ; is_local_return : bool
  }

let cons_args ret =
  let open Ast_pattern in
  map3 (ptyp_arrow __ __ ret) ~f:(fun label typ args -> (label, typ) :: args)
;;

let is_local_return (_ : core_type) = false

let arrow_pattern =
  let open Ast_pattern in
  map3
    (as__ (cons_args (fix (fun ret -> cons_args ret ||| map0 __ ~f:[]))))
    ~f:(fun typ args ret -> { args; ret; is_local_return = is_local_return typ })
;;

let eta_pattern =
  let open Ast_pattern in
  single_expr_payload (pexp_constraint __ arrow_pattern)
;;

let arg_name i = function
  | Nolabel -> Printf.sprintf "__eta_%d" i
  | Labelled label | Optional label -> label
;;

let expand ~loc f { args; ret; is_local_return } =
  let loc = ghostify#location loc in
  let arg_pats =
    List.mapi args ~f:(fun i (label, _) ->
      label, Ast_builder.pvar ~loc (arg_name i label))
  in
  let arg_exprs =
    List.mapi args ~f:(fun i (label, arg) ->
      let arg =
        match label with
        | Nolabel | Labelled _ -> arg
        | Optional _ -> [%type: [%t arg] Stdlib.Option.t]
      in
      label, [%expr ([%e Ast_builder.evar ~loc (arg_name i label)] : [%t arg])])
  in
  let body = { (Ast_builder.pexp_apply ~loc f arg_exprs) with pexp_loc = loc } in
  let ret_expr = [%expr ([%e body] : [%t ret])] in
  let ret_expr = if is_local_return then [%expr [%e ret_expr]] else ret_expr in
  List.fold_right
    arg_pats
    ~f:(fun (label, pat) -> Ast_builder.pexp_fun ~loc label None pat)
    ~init:ret_expr
;;

let eta_extension =
  Extension.declare
    "eta"
    Extension.Context.expression
    eta_pattern
    (fun ~loc ~path:(_ : string) expr arrow -> expand ~loc expr arrow)
;;

let extensions = [ eta_extension ]
