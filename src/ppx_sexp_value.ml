open Base
open Ppxlib
open Ast_builder.Default

let omit_nil =
  Attribute.declare
    "sexp_value.sexp.omit_nil"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let option =
  Attribute.declare
    "sexp_value.sexp.option"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let or_null =
  Attribute.declare
    "sexp_value.sexp.or_null"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let sexp_atom ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.List [%e x]]

let rec list_and_tail_of_ast_list rev_el e =
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree e.pexp_desc ~loc:e.pexp_loc with
  | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc; pexp_loc; _ }) ->
    (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
     | Pexp_tuple [ (None, hd); (None, tl) ] ->
       list_and_tail_of_ast_list (hd :: rev_el) tl
     | _ -> List.rev rev_el, Some e)
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev rev_el, None
  | _ -> List.rev rev_el, Some e
;;

let sexp_of_constant ~loc ~stackify const =
  let stackify_suffix = if stackify then "__stack" else "" in
  let mk typ const =
    eapply
      ~loc
      (evar ~loc ("Ppx_sexp_conv_lib.Conv.sexp_of_" ^ typ ^ stackify_suffix))
      [ pexp_constant ~loc const ]
  in
  let f typ = mk typ const in
  match Ppxlib_jane.Shim.Constant.of_parsetree const with
  | Pconst_integer _ -> f "int"
  | Pconst_char _ -> f "char"
  | Pconst_string _ -> f "string"
  | Pconst_float _ -> f "float"
  | Pconst_unboxed_float (x, c) -> mk "float" (Pconst_float (x, c))
  | Pconst_unboxed_integer (x, c) -> mk "int" (Pconst_integer (x, Some c))
  | Pconst_untagged_char c -> mk "char" (Pconst_char c)
;;

type omittable_sexp =
  | Present of expression
  | Optional of (Location.t * string) * expression * (expression -> expression)
  (* In [Optional (_, e, k)], [e] is an ast whose values have type ['a option], and [k] is
     a function from ast of type ['a] to ast of type [Sexp.t]. The None case should not be
     displayed, and the [a] in the Some case should be displayed by calling [k] on it. *)
  | Nullable of (Location.t * string) * expression * (expression -> expression)
  (* In [Nullable (_, e, k)], [e] is an ast whose values have type ['a or_null], and [k]
     is a function from ast of type ['a] to ast of type [Sexp.t]. The Null case should not
     be displayed, and the [a] in the This case should be displayed by calling [k] on it. *)
  | Omit_nil of Location.t * expression * (expression -> expression)
(* In [Omit_nil (_, e, k)], [e] is an ast of type [Sexp.t], and [k] if a function ast of
   type [Sexp.t] and returns an other [Sexp.t]. When [e] is [List []], it should be not
   displayed. Otherwise [e] should be displayed by calling [k] on it. *)

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Optional (loc, e, k) -> Optional (loc, e, fun e -> f (k e))
  | Nullable (loc, e, k) -> Nullable (loc, e, fun e -> f (k e))
  | Present e -> Present (f e)
  | Omit_nil (loc, e, k) -> Omit_nil (loc, e, fun e -> f (k e))
;;

let sexp_of_constraint ~loc ~stackify expr ctyp =
  match ctyp with
  | [%type: [%t? ty] option] when Option.is_some (Attribute.get option ctyp) ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty ~stackify in
    Optional ((loc, "[@sexp.option]"), expr, fun expr -> eapply ~loc sexp_of [ expr ])
  | [%type: [%t? ty] or_null] when Option.is_some (Attribute.get or_null ctyp) ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty ~stackify in
    Nullable ((loc, "[@sexp.or_null]"), expr, fun expr -> eapply ~loc sexp_of [ expr ])
  | _ ->
    let expr =
      let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp ~stackify in
      eapply ~loc sexp_of [ expr ]
    in
    (match Attribute.get omit_nil ctyp with
     | Some () -> Omit_nil (loc, expr, Fn.id)
     | None -> Present expr)
;;

let is_list_construction : Ppxlib_jane.Shim.Expression_desc.t -> bool = function
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> true
  | Pexp_construct ({ txt = Lident "::"; _ }, Some { pexp_desc; pexp_loc; _ }) ->
    (match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
     | Pexp_tuple [ (None, _); (None, _) ] -> true
     | _ -> false)
  | _ -> false
;;

let rec sexp_of_expr expr ~stackify =
  match omittable_sexp_of_expr expr ~stackify with
  | Present v -> v
  | Optional ((loc, s), _, _) ->
    Location.raise_errorf ~loc "ppx_sexp_value: cannot handle %s in this context" s
  | Nullable ((loc, s), _, _) ->
    Location.raise_errorf ~loc "ppx_sexp_value: cannot handle %s in this context" s
  | Omit_nil (loc, _, _) ->
    Location.raise_errorf ~loc "ppx_sexp_value: cannot handle [@omit_nil] in this context"

and omittable_sexp_of_expr expr ~stackify =
  let loc = { expr.pexp_loc with loc_ghost = true } in
  wrap_sexp_if_present
    ~f:(fun new_expr -> { new_expr with pexp_attributes = expr.pexp_attributes })
    (match
       Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:expr.pexp_loc expr.pexp_desc
     with
     | Pexp_ifthenelse (e1, e2, e3) ->
       Present
         { expr with
           pexp_desc =
             Pexp_ifthenelse
               ( e1
               , sexp_of_expr e2 ~stackify
               , match e3 with
                 | None -> None
                 | Some e -> Some (sexp_of_expr e ~stackify) )
         }
     | Pexp_constraint (expr, Some ctyp, _) -> sexp_of_constraint ~loc ~stackify expr ctyp
     | expr_desc when is_list_construction expr_desc ->
       let el, tl = list_and_tail_of_ast_list [] expr in
       let el = List.map el ~f:(omittable_sexp_of_expr ~stackify) in
       let tl =
         match tl with
         | None -> [%expr []]
         | Some e ->
           [%expr
             match [%e sexp_of_expr e ~stackify] with
             | Ppx_sexp_conv_lib.Sexp.List l -> l
             | Ppx_sexp_conv_lib.Sexp.Atom _ as sexp -> [ sexp ]]
       in
       Present (sexp_of_omittable_sexp_list loc el ~tl)
     | Pexp_constant const -> Present (sexp_of_constant ~loc ~stackify const)
     | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
       Present (sexp_atom ~loc (Ppx_here_expander.lift_position_as_string ~loc))
     | Pexp_extension ({ txt = "string"; _ }, _) -> Present (sexp_atom ~loc expr)
     | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
       Present (sexp_list ~loc (elist ~loc []))
     | Pexp_construct ({ txt = Lident constr; _ }, None) | Pexp_variant (constr, None) ->
       Present (sexp_atom ~loc (estring ~loc constr))
     | Pexp_construct ({ txt = Lident constr; _ }, Some arg)
     | Pexp_variant (constr, Some arg) ->
       let k hole =
         sexp_list ~loc (elist ~loc [ sexp_atom ~loc (estring ~loc constr); hole ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr arg ~stackify) ~f:k
     | Pexp_tuple labeled_el ->
       (match Ppxlib_jane.as_unlabeled_tuple labeled_el with
        | Some el ->
          let el = List.map el ~f:(omittable_sexp_of_expr ~stackify) in
          Present (sexp_of_omittable_sexp_list loc el ~tl:(elist ~loc []))
        | None ->
          Location.raise_errorf ~loc "ppx_sexp_value: labeled tuples are unsupported")
     | Pexp_record (fields, None) -> Present (sexp_of_record ~loc ~stackify fields)
     | Pexp_apply
         ( { pexp_desc = Pexp_ident { txt = Lident "~~"; _ }; _ }
         , [ (Nolabel, { pexp_desc = inner_desc; pexp_loc = inner_loc; _ }) ] ) ->
       (match Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:inner_loc inner_desc with
        | Pexp_constraint (expr, Some ctyp, _) ->
          let expr_str = Pprintast.string_of_expression expr in
          let k hole =
            sexp_list ~loc (elist ~loc [ sexp_atom ~loc (estring ~loc expr_str); hole ])
          in
          wrap_sexp_if_present (sexp_of_constraint ~loc ~stackify expr ctyp) ~f:k
        | _ ->
          Location.raise_errorf
            ~loc
            "ppx_sexp_value: don't know how to handle this construct")
     | _ ->
       Location.raise_errorf
         ~loc
         "ppx_sexp_value: don't know how to handle this construct")

and sexp_of_omittable_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      match e with
      | Present e -> [%expr [%e e] :: [%e acc]]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl]
      | Nullable (_, v_orn, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_orn], [%e acc] with
          | Null, tl -> tl
          | This v, tl -> [%e k [%expr v]] :: tl]
      | Omit_nil (_, e, k) ->
        [%expr
          match [%e e], [%e acc] with
          | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
          | v, tl -> [%e k [%expr v]] :: tl])
  in
  sexp_list ~loc l

and sexp_of_record ~loc ~stackify fields =
  sexp_of_omittable_sexp_list
    loc
    ~tl:(elist ~loc [])
    (List.map fields ~f:(fun (id, e) ->
       let e =
         match
           Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:e.pexp_loc e.pexp_desc
         with
         | Pexp_constraint (e', Some c, modes)
           when Location.compare_pos id.loc.loc_start e.pexp_loc.loc_start = 0
                && Location.compare e.pexp_loc e'.pexp_loc = 0 ->
           (* { foo : int }  *)
           { e with
             pexp_desc =
               Pexp_constraint ({ e' with pexp_loc = id.loc }, Some c, modes)
               |> Ppxlib_jane.Shim.Expression_desc.to_parsetree ~loc:e.pexp_loc
           }
         | _ -> e
       in
       let loc = { id.loc with loc_end = e.pexp_loc.loc_end; loc_ghost = true } in
       let name = String.concat ~sep:"." (Longident.flatten_exn id.txt) in
       let k hole =
         sexp_list
           ~loc
           (elist
              ~loc
              [ sexp_atom ~loc (estring ~loc:{ id.loc with loc_ghost = true } name)
              ; hole
              ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr e ~stackify) ~f:k))
;;

let () =
  Driver.register_transformation
    "sexp_value"
    ~extensions:
      [ Extension.declare
          "sexp"
          Extension.Context.expression
          Ast_pattern.(single_expr_payload __)
          (fun ~loc:_ ~path:_ e -> sexp_of_expr e ~stackify:false)
      ; Extension.declare
          "sexp__stack"
          Extension.Context.expression
          Ast_pattern.(single_expr_payload __)
          (fun ~loc:_ ~path:_ e -> sexp_of_expr e ~stackify:true)
      ; Extension.declare
          "lazy_sexp"
          Extension.Context.expression
          Ast_pattern.(single_expr_payload __)
          (fun ~loc ~path:_ e -> [%expr lazy [%e sexp_of_expr e ~stackify:false]])
      ]
;;
