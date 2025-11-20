open! Stdppx
open! Import
module Ast_builder_jane = Ppxlib_jane.Ast_builder.Default

module Item = struct
  type 'a t =
    | Structure : structure_item t
    | Signature : signature_item t

  let items_pattern : type a. a t -> (a list, _, _) Ast_pattern.t -> _ = function
    | Structure -> Ast_pattern.pstr
    | Signature ->
      let open Ast_pattern in
      fun s -> s |> signature |> psig
  ;;

  let extension_pattern
    : type a. a t -> (extension, _, _) Ast_pattern.t -> _ -> (a, _, _) Ast_pattern.t
    = function
    | Structure -> Ast_pattern.pstr_extension
    | Signature -> Ast_pattern.psig_extension
  ;;

  let decl_pattern : type a. a t -> _ -> _ -> (a, _, _) Ast_pattern.t = function
    | Structure -> Ast_pattern.pstr_type
    | Signature -> Ast_pattern.psig_type
  ;;

  open struct
    let marker =
      object
        inherit Ppxlib.Ast_traverse.iter as super

        method! attribute attr =
          Attribute.mark_as_handled_manually attr;
          super#attribute attr
      end
    ;;
  end

  let mark_attributes : type a. a t -> a list -> unit =
    fun (type a) (t : a t) (as_ : a list) ->
    List.iter as_ ~f:(fun a ->
      match t with
      | Structure -> marker#structure_item a
      | Signature -> marker#signature_item a)
  ;;
end

let type_declaration_deriving_pattern (type a) ~(item : a Item.t) =
  let open Ast_pattern in
  let pat =
    many
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
       |> Item.decl_pattern item __
       |> map ~f:(fun k rec_flag attrs decl -> k (Some (rec_flag, attrs, decl)))
       ||| map drop ~f:(fun k -> k None))
    |> as__
    |> Item.items_pattern item
    |> map' ~f:(fun loc k items type_decl_infos ->
      let error err =
        (* If we don't do this, the compiler prefers to complain about unhandled
           attributes rather than the nice error message we give it. *)
        Item.mark_attributes item items;
        Error err
      in
      let type_decl_infos =
        match List.filter_opt type_decl_infos with
        | [ (rec_flag, attrs, decl) ] ->
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
          (match deriving with
           | [ deriving ] ->
             let deriving = ghostify#payload deriving.attr_payload in
             let decl = ghostify#type_declaration decl in
             Ok (rec_flag, deriving, attrs, decl)
           | [] | _ :: _ :: _ ->
             error
               (Location.error_extensionf
                  ~loc:decl.ptype_loc
                  "Expected exactly one @@@deriving or @@@deriving_inline attribute"))
        | [] | _ :: _ :: _ ->
          error
            (Location.error_extensionf
               ~loc
               "Expected exactly one type declaration with a manifest")
      in
      k ~items ~type_decl_infos)
  in
  (* [ppx_template] expands some payloads to a [[%%template.inline]] node. That node is in
     fact inlined into the surrounding structure/signature, but not until after the
     [[%%rederive]] is expanded. So we specially make [[%%template.inline]] transparent
     here. *)
  Item.extension_pattern item (extension (string "template.inline") pat) drop ^:: nil
  |> Item.items_pattern item
  ||| pat
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

let include_derivings_in_impl ~portable ~loc ~path:(_ : string) ~items ~type_decl_infos =
  match type_decl_infos with
  | Error err -> Ast_builder.pstr_extension ~loc err []
  | Ok (rec_flag, deriving, attrs, decl) ->
    let loc = ghostify#location loc in
    (* We use metaquot for the [include] because it's more convenient than [Ast_builder],
       as includes are especially cumbersome. *)
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

let include_derivings_in_intf ~nonportable ~loc ~path:(_ : string) ~items ~type_decl_infos
  =
  match type_decl_infos with
  | Error err -> [ Ast_builder.psig_extension ~loc err [] ]
  | Ok ((_ : rec_flag), (_ : payload), (_ : attribute list), decl) ->
    let loc = ghostify#location loc in
    [ Ast_builder_jane.psig_include
        ~loc
        ~modalities:
          (if nonportable
           then [ Ast_builder.Located.mk ~loc (Ppxlib_jane.Modality "nonportable") ]
           else [])
        (Ast_builder_jane.include_infos
           ~loc
           ~kind:Structure
           (pmty_with_typesubst ~loc (Ast_builder.pmty_signature ~loc items) decl))
    ]
;;

let intf_extensions =
  [ Extension.declare_inline
      "rederive"
      Signature_item
      (type_declaration_deriving_pattern ~item:Signature)
      (include_derivings_in_intf ~nonportable:false)
  ; Extension.declare_inline
      "@rederive.nonportable"
      Signature_item
      (type_declaration_deriving_pattern ~item:Signature)
      (include_derivings_in_intf ~nonportable:true)
  ]
;;

let extensions = intf_extensions @ impl_extensions
