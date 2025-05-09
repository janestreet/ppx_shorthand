open! Stdppx
open! Import
module Ast_builder_jane = Ppxlib_jane.Ast_builder.Default

module Item = struct
  type 'a t =
    | Structure : structure_item t
    | Signature : signature_item t

  let extension_pattern : type a. a t -> (a list, _, _) Ast_pattern.t -> _ = function
    | Structure -> Ast_pattern.pstr
    | Signature ->
      let open Ast_pattern in
      fun s -> s |> signature |> psig
  ;;

  let decl_pattern : type a. a t -> _ -> _ -> (a, _, _) Ast_pattern.t = function
    | Structure -> Ast_pattern.pstr_type
    | Signature -> Ast_pattern.psig_type
  ;;

  let collect_attributes (type a) (t : a t) (items : a list) =
    List.filter_map items ~f:(fun item ->
      match t, item with
      | Structure, { pstr_desc = Pstr_attribute attr; _ }
      | Signature, { psig_desc = Psig_attribute attr; _ } -> Some attr
      | _ -> None)
  ;;
end

let type_declaration_deriving_pattern (type a) ~(item : a Item.t) =
  let open Ast_pattern in
  (type_declaration_attributes
     __
     (as__
        (type_declaration
           ~name:drop
           ~params:drop
           ~cstrs:drop
           ~kind:drop
           ~private_:drop
           ~manifest:(some drop) (* Required by [with] constraint. *)))
   ^:: nil
   |> Item.decl_pattern item __)
  ^:: drop
  |> as__
  |> Item.extension_pattern item
  |> map ~f:(fun k items rec_flag attrs decl ->
    let deriving, attrs =
      List.partition attrs ~f:(function
        | { attr_name =
              { txt =
                  ( "ppxlib.deriving"
                  | "ppxlib.deriving_inline"
                  | "deriving"
                  | "deriving_inline" )
              ; _
              }
          ; _
          } -> true
        | _ -> false)
    in
    let deriving =
      match deriving with
      | [ deriving ] -> Ok (ghostify#payload deriving.attr_payload)
      | [] | _ :: _ :: _ ->
        (* If we don't do this, the compiler prefers to complain about unhandled
           attributes rather than the nice error message we give it. *)
        let mark_all_as_handled_manually =
          List.iter ~f:Attribute.mark_as_handled_manually
        in
        mark_all_as_handled_manually deriving;
        mark_all_as_handled_manually attrs;
        mark_all_as_handled_manually (Item.collect_attributes item items);
        Error
          (Location.error_extensionf
             ~loc:decl.ptype_loc
             "Expected exactly one @@@deriving or @@@deriving_inline attribute")
    in
    k ~items ~rec_flag ~deriving ~attrs ~decl:(ghostify#type_declaration decl))
;;

let pmty_with_typesubst ~loc mod_type decl =
  Ast_builder.pmty_with
    ~loc
    mod_type
    [ Pwith_typesubst
        ( Ast_builder.Located.map_lident decl.ptype_name
        , { decl with
            (* Using [type_declaration] here permits several malformed [with] constraints
               that wouldn't be accepted by the parser and might even crash the compiler.
               We must adjust the original declaration to be valid in this context. *)
            ptype_cstrs = []
          ; ptype_kind = Ptype_abstract
          ; ptype_private = Public
          } )
    ]
;;

let include_derivings_in_impl
  ~portable
  ~loc
  ~path:(_ : string)
  ~items
  ~rec_flag
  ~deriving
  ~attrs
  ~decl
  =
  match deriving with
  | Error err -> Ast_builder.pstr_extension ~loc err []
  | Ok deriving ->
    let loc = ghostify#location loc in
    (* We use metaquot for the [include] because it's more convenient than
         [Ast_builder], as includes are especially cumbersome. *)
    [%stri
      include
        [%m
        Ast_builder.pmod_constraint
          ~loc
          (Ast_builder.pmod_structure ~loc items)
          (pmty_with_typesubst
             ~loc
             (Ast_builder_jane.pmty_signature
                ~loc
                (Ast_builder_jane.signature
                   ~loc
                   ~modalities:
                     (if portable
                      then
                        [ Ast_builder.Located.mk
                            ~loc
                            (Ppxlib_jane.Shim.Modality.Modality "portable")
                        ]
                      else [])
                   [ Ast_builder.psig_type
                       ~loc
                       rec_flag
                       [ { decl with
                           ptype_attributes =
                             Ast_builder.attribute
                               ~loc
                               ~name:{ txt = "ppxlib.deriving"; loc }
                               ~payload:deriving
                             :: attrs
                         }
                       ]
                   ]))
             decl)]]
;;

let impl_extensions =
  [ Extension.declare
      "rederive"
      Structure_item
      (type_declaration_deriving_pattern ~item:Structure)
      (include_derivings_in_impl ~portable:false)
  ; Extension.declare
      "@rederive.portable"
      Structure_item
      (type_declaration_deriving_pattern ~item:Structure)
      (include_derivings_in_impl ~portable:true)
  ]
;;

let intf_extension =
  Extension.declare
    "rederive"
    Signature_item
    (type_declaration_deriving_pattern ~item:Signature)
    (fun
        ~loc
        ~path:(_ : string)
        ~items
        ~rec_flag:(_ : rec_flag)
        ~deriving
        ~attrs:(_ : attributes)
        ~decl
      ->
       match deriving with
       | Error err -> Ast_builder.psig_extension ~loc err []
       | Ok (_ : payload) ->
         let loc = ghostify#location loc in
         [%sigi:
           include
             [%m
           pmty_with_typesubst ~loc (Ast_builder.pmty_signature ~loc items) decl]])
;;

let extensions = intf_extension :: impl_extensions
