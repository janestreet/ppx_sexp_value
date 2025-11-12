open Core

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let%test_unit "polymorphic variant, variant, list, literal" =
  let module M = struct
    type normal_blop = Blop of int [@@deriving sexp_of]

    type variant_blop =
      [ `Message of string
      | `Blop of int
      ]
    [@@deriving sexp_of]
  end
  in
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:
      (List
         [ M.sexp_of_variant_blop (`Message "string")
         ; M.sexp_of_variant_blop (`Blop 2)
         ; M.sexp_of_normal_blop (Blop 2)
         ])
    ([%sexp [ `Message "string"; `Blop 2; Blop 2 ]] [@alloc a])
;;

let%test_unit "record, if" =
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:
      (List [ List [ Atom "message"; Atom "string" ]; List [ Atom "A.blop"; Atom "1" ] ])
    ([%sexp { message = "string"; A.blop = (if true then 1 else `two) }] [@alloc a])
;;

module A = struct
  type t = int [@@deriving sexp_of ~stackify]
end

let a : A.t = 2

let%test_unit "tuple, explicit types" =
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:(List [ Atom "2"; Atom "1" ])
    ([%sexp (a [@alloc a] : A.t), (lazy 1 : int Lazy.t)] [@alloc a])
;;

let%test_unit "constructed list" =
  let int_list = [ 2; 3 ] in
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:(List [ Atom "one"; Atom "2"; Atom "3" ])
    ([%sexp `one :: (int_list : int list)] [@alloc a])
;;

let%test_unit "strange case doesn't raise an exception" =
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:(List [ Atom "A"; Atom "B" ])
    ([%sexp `A :: `B] [@alloc a])
;;

let%test_unit "sexp.option everywhere except record fields" =
  ([%test_result: Sexp.t] [@alloc a])
    ~expect:
      (List
         [ Atom "A"; List [ Atom "B"; Atom "1" ]; List [ Atom "Some \"D\""; Atom "D" ] ])
    ([%sexp
       `A
       , B (Some 1 : (int option[@sexp.option]))
       , C (None : (int option[@sexp.option]))
       , ~~(Some "D" : (string option[@sexp.option]))
       , ~~(None : (string option[@sexp.option]))]
    [@alloc a])
;;

let%test_unit "sexp.or_null everywhere except record fields" =
  [%test_result: Sexp.t]
    ~expect:
      (List
         [ Atom "A"; List [ Atom "B"; Atom "1" ]; List [ Atom "This \"D\""; Atom "D" ] ])
    [%sexp
      `A
      , B (This 1 : (int or_null[@sexp.or_null]))
      , C (Null : (int or_null[@sexp.or_null]))
      , ~~(This "D" : (string or_null[@sexp.or_null]))
      , ~~(Null : (string or_null[@sexp.or_null]))]
;;

module%test [@name "optional record field via sexp.option"] _ = struct
  let none = None
  let some x = Some x

  let%test_unit "absent" =
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:(List [ List [ Atom "a"; Atom "1" ]; List [ Atom "c"; Atom "3" ] ])
      ([%sexp { a = 1; b = (none : (int option[@sexp.option])); c = 3 }] [@alloc a])
  ;;

  let%test_unit "present" =
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:
        (List
           [ List [ Atom "a"; Atom "1" ]
           ; List [ Atom "b"; Atom "2" ]
           ; List [ Atom "c"; Atom "3" ]
           ])
      ([%sexp { a = 1; b = (some 2 : (int option[@sexp.option])); c = 3 }] [@alloc a])
  ;;

  let%test_unit "all absent" =
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:(List [])
      ([%sexp
         { a = (none : (int option[@sexp.option]))
         ; b = (none : (int option[@sexp.option]))
         }]
      [@alloc a])
  ;;

  let%test_unit "tail as variable name" =
    let tail = Some [ "bar"; "bat" ] in
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:
        (List
           [ List [ Atom "head"; Atom "foo" ]
           ; List [ Atom "tail"; List [ Atom "bar"; Atom "bat" ] ]
           ])
      ([%sexp { head = "foo"; tail : (string list option[@sexp.option]) }] [@alloc a])
  ;;
end

module%test [@name "optional record field via sexp.or_null"] _ = struct
  let null = Null
  let this x = This x

  let%test_unit "absent" =
    [%test_result: Sexp.t]
      ~expect:(List [ List [ Atom "a"; Atom "1" ]; List [ Atom "c"; Atom "3" ] ])
      [%sexp { a = 1; b = (null : (int or_null[@sexp.or_null])); c = 3 }]
  ;;

  let%test_unit "present" =
    [%test_result: Sexp.t]
      ~expect:
        (List
           [ List [ Atom "a"; Atom "1" ]
           ; List [ Atom "b"; Atom "2" ]
           ; List [ Atom "c"; Atom "3" ]
           ])
      [%sexp { a = 1; b = (this 2 : (int or_null[@sexp.or_null])); c = 3 }]
  ;;

  let%test_unit "all absent" =
    [%test_result: Sexp.t]
      ~expect:(List [])
      [%sexp
        { a = (null : (int or_null[@sexp.or_null]))
        ; b = (null : (int or_null[@sexp.or_null]))
        }]
  ;;

  let%test_unit "tail as variable name" =
    let tail = This [ "bar"; "bat" ] in
    [%test_result: Sexp.t]
      ~expect:
        (List
           [ List [ Atom "head"; Atom "foo" ]
           ; List [ Atom "tail"; List [ Atom "bar"; Atom "bat" ] ]
           ])
      [%sexp { head = "foo"; tail : (string list or_null[@sexp.or_null]) }]
  ;;
end

let%test_unit "omit_nil" =
  let[@cold] check (sexp @ m) str =
    [%test_result: string] (Sexp.to_string_hum (Sexp.globalize sexp)) ~expect:str
  in
  check ([%sexp { a = ([ 1 ] : (int list[@omit_nil])) }] [@alloc a]) "((a (1)))";
  check ([%sexp { a = ([] : (int list[@omit_nil])) }] [@alloc a]) "()";
  check ([%sexp A, B ([ 1 ] : (int list[@omit_nil]))] [@alloc a]) "(A (B (1)))";
  check ([%sexp A, B ([] : (int list[@omit_nil]))] [@alloc a]) "(A)"
;;

module%test [@name "expressions and their evaluation"] _ = struct
  let%test_unit "at toplevel" =
    let x = 1 in
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:(List [ Atom "x"; Atom "1" ])
      ([%sexp ~~(x : int)] [@alloc a])
  ;;

  let%test_unit "anywhere" =
    let x = 1
    and y = 2 in
    ([%test_result: Sexp.t] [@alloc a])
      ~expect:
        (List
           [ Atom "message"
           ; List [ Atom "x"; Atom "1" ]
           ; List [ Atom "x + y"; Atom "3" ]
           ])
      ([%sexp "message", ~~(x : int), ~~(x + y : int)] [@alloc a])
  ;;
end

let%test_unit "[%string]" =
  let b = "b" in
  ([%test_result: Sexp.t] [@alloc a])
    ([%sexp [%string "a%{b}c"]] [@alloc a])
    ~expect:[%sexp "abc"]
;;

let _no_warnings_from_merlin_check_about_overlapping_locations () =
  let module Foo = struct
    type t = [ `A ]

    let sexp_of_t _ = Sexp.Atom "A" [@@alloc a]
  end
  in
  let foo = `A in
  let maybe_foo = Some `A in
  ( ([%sexp { foo : Foo.t }] [@alloc a])
  , ([%sexp { foo : [< `A ] }] [@alloc a])
  , ([%sexp { foo : [< Foo.t ] }] [@alloc a])
  , ([%sexp { maybe_foo : Foo.t option }] [@alloc a])
  , ([%sexp { maybe_foo : [< `A ] option }] [@alloc a])
  , ([%sexp { maybe_foo : [< Foo.t ] option }] [@alloc a]) )
  [@exclave_if_stack a]
;;

let%test_unit "[%sexp] is not lazy" =
  let side_effect = ref false in
  let _ = [%sexp (side_effect := true : unit)] [@alloc a] in
  [%test_result: bool] ~expect:true !side_effect
;;]

(* do not template lazy_sexp tests because you cannot have local [Lazy.t]s *)
let%test_unit "[%lazy_sexp] is lazy" =
  let side_effect = ref false in
  let delayed_sexp = [%lazy_sexp (side_effect := true : unit)] in
  [%test_result: bool] ~expect:false !side_effect;
  let _ = Lazy.force delayed_sexp in
  [%test_result: bool] ~expect:true !side_effect
;;

(* do not template lazy_sexp tests because you cannot have local [Lazy.t]s *)
module%test [@name "[%lazy_sexp] output"] _ = struct
  let%test_unit "polymorphic variant, variant, list, literal" =
    let module M = struct
      type normal_blop = Blop of int [@@deriving sexp_of]

      type variant_blop =
        [ `Message of string
        | `Blop of int
        ]
      [@@deriving sexp_of]
    end
    in
    [%test_result: Sexp.t]
      ~expect:
        (List
           [ M.sexp_of_variant_blop (`Message "string")
           ; M.sexp_of_variant_blop (`Blop 2)
           ; M.sexp_of_normal_blop (Blop 2)
           ])
      (force [%lazy_sexp [ `Message "string"; `Blop 2; Blop 2 ]])
  ;;

  let%test_unit "record, if" =
    [%test_result: Sexp.t]
      ~expect:
        (List [ List [ Atom "message"; Atom "string" ]; List [ Atom "A.blop"; Atom "1" ] ])
      (force [%lazy_sexp { message = "string"; A.blop = (if true then 1 else `two) }])
  ;;

  let%test_unit "tuple, explicit types" =
    [%test_result: Sexp.t]
      ~expect:(List [ Atom "2"; Atom "1" ])
      (force [%lazy_sexp (a : A.t), (lazy 1 : int Lazy.t)])
  ;;

  let%test_unit "constructed list" =
    let int_list = [ 2; 3 ] in
    [%test_result: Sexp.t]
      ~expect:(List [ Atom "one"; Atom "2"; Atom "3" ])
      (force [%lazy_sexp `one :: (int_list : int list)])
  ;;

  let%test_unit "strange case doesn't raise an exception" =
    [%test_result: Sexp.t]
      ~expect:(List [ Atom "A"; Atom "B" ])
      (force [%lazy_sexp `A :: `B])
  ;;

  let%test_unit "sexp.option everywhere except record fields" =
    [%test_result: Sexp.t]
      ~expect:
        (List
           [ Atom "A"; List [ Atom "B"; Atom "1" ]; List [ Atom "Some \"D\""; Atom "D" ] ])
      (force
         [%lazy_sexp
           `A
           , B (Some 1 : (int option[@sexp.option]))
           , C (None : (int option[@sexp.option]))
           , ~~(Some "D" : (string option[@sexp.option]))
           , ~~(None : (string option[@sexp.option]))])
  ;;

  let%test_unit "sexp.or_null everywhere except record fields" =
    [%test_result: Sexp.t]
      ~expect:
        (List
           [ Atom "A"; List [ Atom "B"; Atom "1" ]; List [ Atom "This \"D\""; Atom "D" ] ])
      [%sexp
        `A
        , B (This 1 : (int or_null[@sexp.or_null]))
        , C (Null : (int or_null[@sexp.or_null]))
        , ~~(This "D" : (string or_null[@sexp.or_null]))
        , ~~(Null : (string or_null[@sexp.or_null]))]
  ;;
end

let%test_unit "[%sexp] works with ppx_template" =
  let open%template struct
    [@@@kind.default k = (bits64, value)]

    type ('a : k) t = T of 'a [@@deriving sexp_of]
    type ('a : k) not_a_t = Not_t of 'a [@@deriving sexp_of]
  end in
  [%test_result: Sexp.t]
    ~expect:[%sexp (T 1 : int t), (Not_t 2 : int not_a_t)]
    [%sexp
      (T #1L : (Int64_u.t t[@kind bits64]))
      , (Not_t #2L : (Int64_u.t not_a_t[@kind bits64]))]
;;
