open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]

let allow_deprecated_syntax = ref false

let sexp_atom ~loc x = [%expr Sexplib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Sexplib.Sexp.List [%e x]]

let rec list_and_tail_of_ast_list rev_el e =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ },
                    Some { pexp_desc = Pexp_tuple [hd; tl]; _ }) ->
    list_and_tail_of_ast_list (hd :: rev_el) tl
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev rev_el, None
  | _ -> List.rev rev_el, Some e
;;

let sexp_of_constant ~loc const =
  let f typ =
    eapply ~loc (evar ~loc ("Sexplib.Conv.sexp_of_" ^ typ)) [pexp_constant ~loc const]
  in
  match const with
  | Const_int       _ -> f "int"
  | Const_char      _ -> f "char"
  | Const_string    _ -> f "string"
  | Const_float     _ -> f "float"
  | Const_int32     _ -> f "int32"
  | Const_int64     _ -> f "int64"
  | Const_nativeint _ -> f "nativeint"
;;

let sexp_of_constraint ~loc expr ctyp =
  let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
  eapply ~loc sexp_of [expr]
;;

let rec sexp_of_expr expr =
  let loc = expr.pexp_loc in
  let new_expr =
    match expr.pexp_desc with
    | Pexp_ifthenelse (e1, e2, e3) ->
      { expr with
        pexp_desc =
          Pexp_ifthenelse (e1, sexp_of_expr e2,
                           match e3 with
                           | None -> None
                           | Some e -> Some (sexp_of_expr e))
      }
    | Pexp_constraint (expr, ctyp) ->
      sexp_of_constraint ~loc expr ctyp
    | Pexp_construct ({ txt = Lident "[]"; _ }, None)
    | Pexp_construct ({ txt = Lident "::"; _ },
                      Some { pexp_desc = Pexp_tuple [_; _]; _ }) ->
      let el, tl = list_and_tail_of_ast_list [] expr in
      let el = List.map el ~f:sexp_of_expr in
      let tl =
        match tl with
        | None -> [%expr [] ]
        | Some e ->
          [%expr
            match [%e sexp_of_expr e] with
            | Sexplib.Sexp.List l -> l
            | Sexplib.Sexp.Atom _ as sexp -> [sexp]
          ]
      in
      sexp_of_sexp_list loc el ~tl
    | Pexp_constant const ->
      sexp_of_constant ~loc const
    | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
      sexp_atom ~loc (Ppx_here_expander.lift_position_as_string ~loc)
    | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
      sexp_list ~loc (elist ~loc [])
    | Pexp_construct ({ txt = Lident constr; _ }, None)
    | Pexp_variant   (               constr     , None) ->
      sexp_atom ~loc (estring ~loc constr)
    | Pexp_construct ({ txt = Lident constr; _ }, Some arg)
    | Pexp_variant   (               constr     , Some arg) ->
      sexp_list ~loc
        (elist ~loc [ sexp_atom ~loc (estring ~loc constr)
                    ; sexp_of_expr arg
                    ])
    | Pexp_tuple el ->
      let el = List.map el ~f:sexp_of_expr in
      sexp_of_sexp_list loc el ~tl:(elist ~loc [])
    | Pexp_record (fields, None) ->
      sexp_of_record ~loc fields
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "~~"; _ }; _},
                  [ ("", { pexp_desc = Pexp_constraint (expr, ctyp); _ }) ]) ->
      let expr_str = Pprintast.string_of_expression expr in
      sexp_list ~loc
        (elist ~loc [ sexp_atom ~loc (estring ~loc expr_str)
                    ; sexp_of_constraint ~loc expr ctyp
                    ])
    | _ ->
      Location.raise_errorf ~loc
        "ppx_sexp_value: don't know how to handle this construct"
  in
  { new_expr with pexp_attributes = expr.pexp_attributes }

and sexp_of_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      [%expr [%e e] :: [%e acc] ])
  in
  [%expr Sexplib.Sexp.List [%e l] ]

and sexp_of_record ~loc fields =
  let rec convert_record_fields = function
    | [] -> [%expr []]
    | (id, e) :: rest ->
      let loc = { id.loc with loc_end = e.pexp_loc.loc_end } in
      let name = String.concat ~sep:"." (Longident.flatten id.txt) in
      let s = estring ~loc:id.loc name in
      let convert_record_field e =
        let sexp = sexp_of_expr e in
        sexp_list ~loc (elist ~loc [ sexp_atom ~loc s; sexp ])
      in
      match e with
      | [%expr ([%e? e] : [%t? ty] sexp_option)] ->
        (* We make sure we convert from left-to-right, so that we always report the
           leftmost error, which is what users expect. *)
        let sexp_hd = convert_record_field [%expr (hd : [%t ty])] in
        let sexp_tl = convert_record_fields rest in
        [%expr
           (* let-and to avoid shadowing variables in the environment of [e] and
              [sexp_tl] with our own bindings. *)
          let tail = [%e sexp_tl]
          and hd_opt = [%e e]
          in
          match hd_opt with
          | None -> tail
          | Some hd -> [%e sexp_hd] :: tail
        ]
      | _ ->
        let sexp_hd = convert_record_field e in
        let sexp_tl = convert_record_fields rest in
        [%expr [%e sexp_hd] :: [%e sexp_tl] ]
  in
  sexp_list ~loc (convert_record_fields fields)
;;

(* Deprecated since 2015-12. *)
module Deprecated = struct
  let hint =
    "See this page for details:\n\
     https://github.com/janestreet/ppx_sexp_value"

  let rewrite_arg e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_constraint _ -> eapply ~loc (evar ~loc "~~") [e]
    | _ -> e
  ;;

  let add_warning e msg =
    let attr = Ast_mapper.attribute_of_warning e.pexp_loc msg in
    { e with pexp_attributes = attr :: e.pexp_attributes }
  ;;

  let rewrite_expr expr =
    let loc = expr.pexp_loc in
    match expr.pexp_desc with
    (* Don't misinterpret [%sexp ~~(e : t)] for the deprecated application syntax. *)
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "~~"; _}; _ }, _) -> expr
    | Pexp_apply (f, (_ :: _ as args))
      when List.for_all args ~f:(fun (lab, _) -> lab = "") ->
      let el = List.map (f :: List.map args ~f:snd) ~f:rewrite_arg in
      let e = pexp_tuple ~loc el in
      if !allow_deprecated_syntax then
        e
      else
        add_warning e
          ("ppx_sexp_value: the application syntax is deprecated.\n" ^ hint)
    | _ -> expr
  ;;

  let add_deprecated e =
    if !allow_deprecated_syntax then
      e
    else
      add_warning e
        ("ppx_sexp_value: deprecated extension, use [%sexp] instead.\n" ^ hint)
  ;;

  let expand_structural_sexp ~loc:_ ~path:_ e =
    add_deprecated (sexp_of_expr (rewrite_expr e))
  ;;

  let expand_error func_name ~loc:_ ~path:_ loc e1 e2 =
    add_deprecated
      (eapply ~loc (evar ~loc func_name)
         [e1; [%expr ()]; [%expr fun () -> [%e sexp_of_expr e2]]])
  ;;
end

let expand ~loc ~path e =
  if !allow_deprecated_syntax then
    Deprecated.expand_structural_sexp ~loc ~path e
  else
    (* Rewrite to make sure we get the warning and not an error *)
    let e = Deprecated.rewrite_expr e in
    sexp_of_expr e
;;

let extensions =
  let one_expr  =
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
  and two_exprs =
    Ast_pattern.(pstr (pstr_eval (pexp_loc __ (pexp_apply __ ((no_label __) ^:: nil))) nil
                       ^:: nil))
  in
  let declare name patt k =
    Extension.V2.declare name Extension.Context.expression patt k
  in
  [ declare "sexp"                  one_expr  expand
  ; declare "structural_sexp"       one_expr   Deprecated.expand_structural_sexp
  ; declare "raise_structural_sexp" two_exprs (Deprecated.expand_error "Error.failwiths")
  ; declare "structural_error"      two_exprs (Deprecated.expand_error "Error.create"   )
  ; declare "structural_or_error"   two_exprs (Deprecated.expand_error "Or_error.error" )
  ]
;;

let () =
  Ppx_driver.register_transformation "sexp_value"
    ~extensions
;;
