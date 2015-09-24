open Core.Std

let%test_unit "polymorphic variant, variant, list, literal" =
  let module M = struct
    type normal_blop = Blop of int [@@deriving sexp_of]
    type variant_blop = [ `Message of string | `Blop of int ] [@@deriving sexp_of]
  end in
  [%test_result: Sexp.t]
    ~expect:(List [
      M.sexp_of_variant_blop (`Message "string");
      M.sexp_of_variant_blop (`Blop 2);
      M.sexp_of_normal_blop (Blop 2);
    ])
    [%sexp [ `Message "string"; `Blop 2; Blop 2 ]]
;;

let%test_unit "record, if" =
  [%test_result: Sexp.t]
    ~expect:(List [
      List [Atom "message"; Atom "string"];
      List [Atom "A.blop"; Atom "1";]
    ])
    [%sexp { message = "string"; A.blop = if true then 1 else `two; }]
;;

module A = struct
  type t = int [@@deriving sexp_of]
end

let a : A.t = 2

let%test_unit "tuple, explicit types" =
  [%test_result: Sexp.t]
    ~expect:(List [Atom "2"; Atom "1"])
    [%sexp (a : A.t), (lazy 1 : int Lazy.t)]
;;

let%test_unit "constructed list" =
  let int_list = [2; 3] in
  [%test_result: Sexp.t]
    ~expect:(List [Atom "one"; Atom "2"; Atom "3"])
    [%sexp `one :: (int_list : int list)]
;;

let%test_unit "strange case doesn't raise an exception" =
  [%test_result: Sexp.t]
    ~expect:(List [Atom "A"; Atom "B"])
    [%sexp `A :: `B]
;;

let%test_module "optional record field" =
  (module struct
    let%test_unit "absent" =
      [%test_result: Sexp.t]
        ~expect:(List [ List [ Atom "a"; Atom "1" ]
                      ; List [ Atom "c"; Atom "3" ]])
        [%sexp { a = 1; b = (None : int sexp_option); c = 3; }]
    ;;

    let%test_unit "present" =
      [%test_result: Sexp.t]
        ~expect:(List [ List [ Atom "a"; Atom "1" ]
                      ; List [ Atom "b"; Atom "2" ]
                      ; List [ Atom "c"; Atom "3" ]])
        [%sexp { a = 1; b = (Some 2 : int sexp_option); c = 3; }]
    ;;
  end)
;;

let%test_module "expressions and their evaluation" =
  (module struct
    let%test_unit "one expression" =
      let x = 1 and y = 2 in
      [%test_result: Sexp.t]
        ~expect:(Atom "3")
        [%sexp (x + y : int)]
    ;;

    let%test_unit "several expressions" =
      let x = 1 and y = 2 in
      [%test_result: Sexp.t]
        ~expect:(List [ Atom "message"
                      ; List [ Atom "x";     Atom "1" ]
                      ; List [ Atom "x + y"; Atom "3" ]
                      ])
        [%sexp "message" (x : int) (x + y : int)]
    ;;
  end)
;;

module Other_quotation_expanders = struct

  let test_exn here f =
    try f ()
    with e ->
      [%test_result: string] ~here:[here]
        (Exn.to_string e)
        ~expect:"(message ((value 2)))"

  let%test_unit _ =
    test_exn _here_ (fun () -> [%raise_structural_sexp "message" { value = 2 }])
  ;;

  let%test_unit _ =
    test_exn _here_ (fun () -> Error.raise [%structural_error "message" { value = 2 }])
  ;;

  let%test_unit _ =
    test_exn _here_ (fun () -> ok_exn [%structural_or_error "message" { value = 2 }])
  ;;
end
