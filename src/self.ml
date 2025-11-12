open! Stdppx
open Ppxlib

(* Gives an unused open warning only if neither are used. *)
open struct
  include Ast_builder.Default
  include Ppxlib_jane.Ast_builder.Default
end

let rec final_name_of_longident = function
  | Lident name -> name
  | Ldot (_, name) -> name
  | Lapply (id, _) -> final_name_of_longident id
;;

let module_type_of_package_type ~type_loc (id, alist) =
  let self = pmty_ident ~loc:id.loc id in
  match List.is_empty alist with
  | true -> self
  | false ->
    List.map alist ~f:(fun (name, ty) ->
      Pwith_type
        ( name
        , type_declaration
            ~loc:ty.ptyp_loc
            ~name:(Loc.map name ~f:final_name_of_longident)
            ~params:[]
            ~cstrs:[]
            ~kind:Ptype_abstract
            ~private_:Public
            ~manifest:(Some ty)
            () ))
    |> pmty_with ~loc:type_loc self
;;

let ghost =
  object
    inherit Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end
;;

type kind =
  | Module of module_type
  | Value of package_type loc

let self_expander ~ctxt name kind =
  let ext_loc = Expansion_context.Extension.extension_point_loc ctxt in
  let fresh =
    gen_symbol ~prefix:(String.capitalize_ascii name.txt) () |> Located.mk ~loc:name.loc
  in
  let fresh_module = pmod_ident ~loc:name.loc (Located.map_lident fresh) in
  let ty =
    match kind with
    | Module ty -> ty
    | Value ty -> module_type_of_package_type ~type_loc:ty.loc ty.txt
  in
  let body =
    match kind with
    | Module ty ->
      pstr_module
        ~loc:ext_loc
        (module_binding
           ~loc:ext_loc
           ~name:(Loc.map name ~f:(fun s -> Some s))
           ~expr:(pmod_constraint ~loc:ext_loc fresh_module (Some ty) []))
    | Value ty ->
      pstr_value
        ~loc:ext_loc
        Nonrecursive
        [ value_binding
            ~loc:ext_loc
            ~pat:(pvar ~loc:name.loc name.txt)
            ~expr:
              (pexp_constraint
                 ~loc:ext_loc
                 (pexp_pack ~loc:name.loc fresh_module)
                 (Some (ptyp_package ~loc:ty.loc ty.txt))
                 [])
            ~modes:[]
        ]
  in
  pmod_structure ~loc:ext_loc [ body ]
  |> ghost#module_expr
  |> pmod_functor
       ~loc:ext_loc
       (Named (Loc.map fresh ~f:(fun x -> Some x), ty, [])
        |> Ppxlib_jane.Shim.Functor_parameter.to_parsetree)
  |> include_infos ~loc:ext_loc ~kind:Functor
  |> pstr_include ~loc:ext_loc
;;

let self_pattern =
  let open Ast_pattern in
  let signature = signature in
  let module_pattern =
    module_declaration ~name:(some __') ~type_:(map1 __ ~f:(fun ty -> Module ty))
    |> psig_module
  in
  let value_pattern =
    value_description
      ~name:__'
      ~type_:(ptyp_package (map1 __' ~f:(fun ty -> Value ty)))
      ~prim:nil
    |> psig_value
  in
  psig (signature ((module_pattern ||| value_pattern) ^:: nil))
;;

let self_extension = Extension.V3.declare "self" Structure_item self_pattern self_expander
let extensions = [ self_extension ]
