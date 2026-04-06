open! Stdppx
open! Import

let eta = "eta"

type t =
  | Eta
  | Eta_n of { n : int }

let extension_name = function
  | Eta -> eta
  | Eta_n { n } -> eta ^ Int.to_string n
;;

let arg_name i = Printf.sprintf "__eta_%d" i
let pexp_ident str ~loc = Ppxlib.Loc.make (Lident str) ~loc |> Ast_builder.pexp_ident ~loc
let pexp_hole_reserved_name_for_non_oxcaml_code = "ppx_eta_internal__pexp_hole_shim"

let is_pexp_hole pexp_desc ~loc =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
  | Pexp_hole -> true
  | Pexp_ident { txt = Lident name; _ }
    when String.equal name pexp_hole_reserved_name_for_non_oxcaml_code -> true
  | _ -> false
;;

(* We replace each hole with an identifier [__eta_N], where [N] is the number argument of
   the hole (starting from [0]) in the original function application expression. Note that
   this scheme is hygienic even in the face of nested [[%eta]]s, because each identifier
   used to replace a hole is bound in the immediately surrounding abstraction. *)

(* [f a b _ c ~x:_ (_ : int)] returns arg pats [__eta_2 ~x:__eta_4 (__eta_5 : int)]
*)
let arg_pats args ~loc =
  args
  |> List.mapi ~f:(fun i (label, { pexp_desc; _ }) ->
    let eta_arg_pat = Ast_builder.pvar (arg_name i) ~loc in
    if is_pexp_hole pexp_desc ~loc
    then Some (label, eta_arg_pat)
    else (
      match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
      | Pexp_constraint ({ pexp_desc; _ }, type_, modes) when is_pexp_hole pexp_desc ~loc
        ->
        let ppat_constraint =
          Ppxlib_jane.Ast_builder.Default.ppat_constraint ~loc eta_arg_pat type_ modes
        in
        Some (label, ppat_constraint)
      | _ -> None))
  |> List.filter_opt
;;

(* [f a b _ c ~x:_ (_ : int)] returns arg exprs [a b __eta_2 c ~x:__eta_4 __eta_5]

   Note this function drops constraints on expressions, but these constraints remain on
   the patterns in [arg_pats].
*)
let arg_exprs args ~loc =
  List.mapi args ~f:(fun i (label, ({ pexp_desc; _ } as arg)) ->
    let eta_arg_expr = pexp_ident (arg_name i) ~loc in
    let arg =
      if is_pexp_hole pexp_desc ~loc
      then eta_arg_expr
      else (
        match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
        | Pexp_constraint ({ pexp_desc; _ }, _, _) when is_pexp_hole pexp_desc ~loc ->
          eta_arg_expr
        | _ -> arg)
    in
    label, arg)
;;

(* [f a b _ c ~x:_ (_ : int)] returns the eta expanded
   [fun arg_pats -> (f arg_exprs : ret)]
*)
let expand ~id ~args ~is_local_return ~ret ~loc =
  let body =
    let args = arg_exprs args ~loc in
    { (Ast_builder.pexp_apply id args ~loc) with pexp_loc = loc }
  in
  let ret_expr =
    match ret with
    | None -> body
    | Some (ret, modes) ->
      Ppxlib_jane.Ast_builder.Default.pexp_constraint ~loc body (Some ret) modes
  in
  let ret_expr = if is_local_return then [%expr exclave_ [%e ret_expr]] else ret_expr in
  [%expr
    [%e
      args
      |> arg_pats ~loc
      |> List.fold_right
           ~f:(fun (label, pat) -> Ast_builder.pexp_fun label None pat ~loc)
           ~init:ret_expr]
    [@inline]]
;;

let maybe_exclave { pexp_desc; _ } ~loc =
  match pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_extension (extension, _); _ }, (Nolabel, f) :: args)
    when String.equal extension.txt "extension.exclave" ->
    Some (Ast_builder.pexp_apply f args ~loc)
  | _ -> None
;;

let raise_invalid_input ~loc =
  Location.raise_errorf
    ~loc
    "ppx_eta: Must pass function application expression that is optionally constrained \
     to a return type. For example [[%%eta (f _ ~y:_ ?z:_ : 'a)]]. If you want a \
     local-returning function, prepend it with [exclave_]: [[%%eta exclave_ f _]]."
;;

let extension t =
  let name = extension_name t in
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:(_ : string) expr ->
      let loc = ghostify#location loc in
      let ({ pexp_desc; _ } as expr), is_local_return =
        match maybe_exclave expr ~loc with
        | None -> expr, false
        | Some expr -> expr, true
      in
      match t with
      | Eta ->
        (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
         | Pexp_apply (id, args) -> expand ~id ~args ~is_local_return ~ret:None ~loc
         | Pexp_constraint ({ pexp_desc = Pexp_apply (id, args); _ }, ret, modes) ->
           expand
             ~id
             ~args
             ~is_local_return
             ~ret:(Option.map ret ~f:(fun ret -> ret, modes))
             ~loc
         | _ -> raise_invalid_input ~loc)
      | Eta_n { n } ->
        let holes =
          List.init ~len:n ~f:(Fun.const (Nolabel, Ast_builder.pexp_hole ~loc))
        in
        (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
         | Pexp_constraint (expr, ret, modes) ->
           expand
             ~id:expr
             ~args:holes
             ~is_local_return
             ~ret:(Option.map ret ~f:(fun ret -> ret, modes))
             ~loc
         | _ -> expand ~id:expr ~args:holes ~is_local_return ~ret:None ~loc))
;;

let extensions =
  Eta :: List.init ~len:3 ~f:(fun i -> Eta_n { n = i + 1 }) |> List.map ~f:extension
;;
