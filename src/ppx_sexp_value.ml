open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default

[@@@metaloc loc]

type sexp_payload =
  | Sexp  of expression
  | Error of string (* function name *) * expression * expression
;;

module E = struct
  let one_expr  = Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
  let two_exprs = Ast_pattern.(pstr (pstr_eval (pexp_apply __ ((no_label __) ^:: nil)) nil
                                     ^:: nil))

  let all =
    let error name x y = Error (name, x, y) in
    let declare name patt k =
      Extension.declare name Extension.Context.expression patt k
    in
    [ declare "sexp"                  one_expr  (fun x -> Sexp x            )
    ; declare "structural_sexp"       one_expr  (fun x -> Sexp x            )
    ; declare "raise_structural_sexp" two_exprs (error "Error.failwiths"    )
    ; declare "structural_error"      two_exprs (error "Error.create"       )
    ; declare "structural_or_error"   two_exprs (error "Or_error.error"     )
    ]
  ;;
end

let rec list_and_tail_of_ast_list rev_el e =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ },
                    Some { pexp_desc = Pexp_tuple [hd; tl]; _ }) ->
    list_and_tail_of_ast_list (hd :: rev_el) tl
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev rev_el, None
  | _ -> List.rev rev_el, Some e
;;

let rec sexp_of_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_ifthenelse (e1, e2, e3) ->
    { expr with
      pexp_desc =
        Pexp_ifthenelse (e1, sexp_of_expr e2,
                         match e3 with
                         | None -> None
                         | Some e -> Some (sexp_of_expr e))
    }
  | Pexp_constraint (e2, ctyp) ->
    let e1 = Ppx_sexp_conv_expander.sexp_of ctyp in
    eapply ~loc e1 [e2]
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
  | Pexp_constant (Const_int i) ->
    [%expr Sexplib.Conv.sexp_of_int [%e eint ~loc i] ]
  | Pexp_constant (Const_string (s, None)) ->
    [%expr Sexplib.Conv.sexp_of_string [%e estring ~loc s] ]
  | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
    [%expr  Sexplib.Sexp.List [] ]
  | Pexp_construct ({ txt = Lident constr; _ }, None)
  | Pexp_variant   (               constr     , None) ->
    [%expr  Sexplib.Sexp.Atom [%e estring ~loc constr] ]
  | Pexp_construct ({ txt = Lident constr; _ }, Some arg)
  | Pexp_variant   (               constr     , Some arg) ->
    [%expr  Sexplib.Sexp.List
              [ Sexplib.Sexp.Atom [%e estring ~loc constr]
              ; [%e sexp_of_expr arg]
              ]
    ]
  | Pexp_tuple el ->
    let el = List.map el ~f:sexp_of_expr in
    sexp_of_sexp_list loc el ~tl:[%expr [] ]
  | Pexp_record (fields, None) ->
    let l =
      List.fold_right fields ~init:[%expr []] ~f:(fun (id, e) tail ->
        let loc = { id.loc with loc_end = e.pexp_loc.loc_end } in
        let name = String.concat ~sep:"." (Longident.flatten id.txt) in
        let s = estring ~loc:id.loc name in
        let convert_record_field e =
          let sexp = sexp_of_expr e in
          [%expr Sexplib.Sexp.List [ Sexplib.Sexp.Atom [%e s]; [%e sexp] ] ]
        in
        match e with
        | [%expr ([%e? e] : [%t? ty] sexp_option)] ->
          [%expr
           let tail = [%e tail]
           and hd_opt = [%e e]
           in
           match hd_opt with
           | None -> tail
           | Some hd -> [%e convert_record_field [%expr (hd : [%t ty])]] :: tail]
        | _ ->
          [%expr [%e convert_record_field e] :: [%e tail]])
    in
    [%expr Sexplib.Sexp.List [%e l] ]
  | _ ->
    Location.raise_errorf ~loc
      "ppx_sexp_value: don't know how to handle this construct"

and sexp_of_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      [%expr [%e e] :: [%e acc] ])
  in
  [%expr Sexplib.Sexp.List [%e l] ]

let toplevel_sexp_of_arg expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_constraint (e2, ctyp) ->
    let e1 = Ppx_sexp_conv_expander.sexp_of ctyp in
    ignore (Format.flush_str_formatter () : string);
    Pprintast.expression Format.str_formatter e2;
    let e2_str = Format.flush_str_formatter () in
    [%expr Sexplib.Sexp.List [ Sexplib.Sexp.Atom [%e estring ~loc e2_str]
                             ; [%e eapply ~loc e1 [e2]]
                             ]
    ]
  | _ -> sexp_of_expr expr

let toplevel_sexp_of_expr expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_apply (f, (_ :: _ as args))
    when List.for_all args ~f:(fun (lab, _) -> lab = "") ->
    let el = List.map (f :: List.map args ~f:snd) ~f:toplevel_sexp_of_arg in
    sexp_of_sexp_list loc el ~tl:[%expr []]
  | _ -> sexp_of_expr expr

let map = object
  inherit Ast_traverse.map as super

  method! expression e =
    let e = super#expression e in
    match e.pexp_desc with
    | Pexp_extension ext -> begin
      match Extension.convert E.all ext with
      | None -> e
      | Some ext ->
        match ext with
        | Sexp e -> toplevel_sexp_of_expr e
        | Error (func_name, e1, e2) ->
          let loc = e.pexp_loc in
          let e' =
            eapply ~loc (evar ~loc func_name) [e1; [%expr ()];
                                               [%expr fun () -> [%e sexp_of_expr e2] ]] in
          { e with pexp_desc = e'.pexp_desc }
      end
    | _ -> e
end

let () =
  Ppx_driver.register_code_transformation
    ~name:"sexp_value"
    ~impl:map#structure
    ~intf:map#signature
;;
