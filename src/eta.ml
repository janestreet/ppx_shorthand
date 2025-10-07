open! Stdppx
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

let is_local_mode mode =
  match mode.txt with
  | Mode "local" -> true
  | Mode _ -> false
;;

let rec is_local_return core_type =
  match core_type.ptyp_desc with
  | Ptyp_arrow (_, _, return_type, _, return_modes) ->
    (match return_type.ptyp_desc with
     | Ptyp_arrow _ -> is_local_return return_type
     | _ -> List.exists return_modes ~f:is_local_mode)
  | _ -> false
;;

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
  let ret_expr = if is_local_return then [%expr exclave_ [%e ret_expr]] else ret_expr in
  [%expr
    [%e
      List.fold_right
        arg_pats
        ~f:(fun (label, pat) -> Ast_builder.pexp_fun ~loc label None pat)
        ~init:ret_expr]
    [@inline]]
;;

let eta_extension =
  Extension.declare
    "eta"
    Extension.Context.expression
    eta_pattern
    (fun ~loc ~path:(_ : string) expr arrow ->
       let loc = ghostify#location loc in
       expand ~loc expr arrow)
;;

let eta_n_pattern =
  let open Ast_pattern in
  single_expr_payload __
;;

let make_expand_n ~num_args ~is_local_return =
  let extension_name = "eta" ^ Int.to_string num_args in
  let extension_name =
    if is_local_return then "@" ^ extension_name ^ ".exclave" else extension_name
  in
  Extension.declare
    extension_name
    Extension.Context.expression
    eta_n_pattern
    (fun ~loc ~path:(_ : string) expr ->
       let loc = ghostify#location loc in
       let arrow =
         { args = List.init ~len:num_args ~f:(Fun.const (Nolabel, [%type: _]))
         ; ret = [%type: _]
         ; is_local_return
         }
       in
       expand ~loc expr arrow)
;;

let cartesian_product_map2 a b ~f =
  List.concat_map a ~f:(fun x -> List.map b ~f:(fun y -> f x y))
;;

let eta_n_extensions =
  cartesian_product_map2
    [ true; false ]
    (List.init ~len:3 ~f:(fun x -> x + 1))
    ~f:(fun is_local_return num_args -> make_expand_n ~is_local_return ~num_args)
;;

let extensions = eta_extension :: eta_n_extensions
