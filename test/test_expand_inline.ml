open Core

(* This file expands [%sexp__stack] inline on a handful of examples from test.ml.

   We just want to spot-check that [%sexp__stack] does in fact use [sexp_of_t__stack] or
   similar functions.
*)

module A = struct
  type t = int [@@deriving sexp_of]
end

let a : A.t = 2
let none = None

[@@@expand_inline
  let _ = [%sexp [ `Message "string"; `Blop 2; Blop 2 ]]
  let _ = [%sexp { message = "string"; A.blop = (if true then 1 else `two) }]
  let _ = [%sexp (a : A.t), (lazy 1 : int Lazy.t)]

  let _ =
    let int_list = [ 2; 3 ] in
    [%sexp `one :: (int_list : int list)]
  ;;

  let _ = [%sexp `A :: `B]

  let _ =
    [%sexp
      `A
      , B (Some 1 : (int option[@sexp.option]))
      , C (None : (int option[@sexp.option]))
      , ~~(Some "D" : (string option[@sexp.option]))
      , ~~(None : (string option[@sexp.option]))]
  ;;

  let _ =
    [%sexp
      { a = (none : (int option[@sexp.option])); b = (none : (int option[@sexp.option])) }]
  ;;

  let _ =
    let tail = Some [ "bar"; "bat" ] in
    [%sexp { head = "foo"; tail : (string list option[@sexp.option]) }]
  ;;

  let _ =
    let b = "b" in
    [%sexp [%string "a%{b}c"]]
  ;;

  let _ =
    let side_effect = ref false in
    [%sexp (side_effect := true : unit)]
  ;;]

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    [ Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "Message"
        ; Ppx_sexp_conv_lib.Conv.sexp_of_string "string"
        ]
    ; Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "Blop"; Ppx_sexp_conv_lib.Conv.sexp_of_int 2 ]
    ; Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "Blop"; Ppx_sexp_conv_lib.Conv.sexp_of_int 2 ]
    ]
;;

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    [ Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "message"
        ; Ppx_sexp_conv_lib.Conv.sexp_of_string "string"
        ]
    ; Ppx_sexp_conv_lib.Sexp.List
        [ Ppx_sexp_conv_lib.Sexp.Atom "A.blop"
        ; (if true
           then Ppx_sexp_conv_lib.Conv.sexp_of_int 1
           else Ppx_sexp_conv_lib.Sexp.Atom "two")
        ]
    ]
;;

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    [ (A.sexp_of_t [@merlin.hide]) a
    ; (fun [@merlin.hide] x__001_ -> Lazy.sexp_of_t sexp_of_int x__001_) (lazy 1)
    ]
;;

let _ =
  let int_list = [ 2; 3 ] in
  Ppx_sexp_conv_lib.Sexp.List
    (Ppx_sexp_conv_lib.Sexp.Atom "one"
     ::
     (match (fun [@merlin.hide] x__002_ -> sexp_of_list sexp_of_int x__002_) int_list with
      | Ppx_sexp_conv_lib.Sexp.List l -> l
      | Ppx_sexp_conv_lib.Sexp.Atom _ as sexp -> [ sexp ]))
;;

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    (Ppx_sexp_conv_lib.Sexp.Atom "A"
     ::
     (match Ppx_sexp_conv_lib.Sexp.Atom "B" with
      | Ppx_sexp_conv_lib.Sexp.List l -> l
      | Ppx_sexp_conv_lib.Sexp.Atom _ as sexp -> [ sexp ]))
;;

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    (Ppx_sexp_conv_lib.Sexp.Atom "A"
     ::
     (match
        ( Some 1
        , match
            ( None
            , match
                ( Some "D"
                , match None, [] with
                  | None, tl -> tl
                  | Some v, tl ->
                    Ppx_sexp_conv_lib.Sexp.List
                      [ Ppx_sexp_conv_lib.Sexp.Atom "None"
                      ; (sexp_of_string [@merlin.hide]) v
                      ]
                    :: tl )
              with
              | None, tl -> tl
              | Some v, tl ->
                Ppx_sexp_conv_lib.Sexp.List
                  [ Ppx_sexp_conv_lib.Sexp.Atom "Some \"D\""
                  ; (sexp_of_string [@merlin.hide]) v
                  ]
                :: tl )
          with
          | None, tl -> tl
          | Some v, tl ->
            Ppx_sexp_conv_lib.Sexp.List
              [ Ppx_sexp_conv_lib.Sexp.Atom "C"; (sexp_of_int [@merlin.hide]) v ]
            :: tl )
      with
      | None, tl -> tl
      | Some v, tl ->
        Ppx_sexp_conv_lib.Sexp.List
          [ Ppx_sexp_conv_lib.Sexp.Atom "B"; (sexp_of_int [@merlin.hide]) v ]
        :: tl))
;;

let _ =
  Ppx_sexp_conv_lib.Sexp.List
    (match
       ( none
       , match none, [] with
         | None, tl -> tl
         | Some v, tl ->
           Ppx_sexp_conv_lib.Sexp.List
             [ Ppx_sexp_conv_lib.Sexp.Atom "b"; (sexp_of_int [@merlin.hide]) v ]
           :: tl )
     with
     | None, tl -> tl
     | Some v, tl ->
       Ppx_sexp_conv_lib.Sexp.List
         [ Ppx_sexp_conv_lib.Sexp.Atom "a"; (sexp_of_int [@merlin.hide]) v ]
       :: tl)
;;

let _ =
  let tail = Some [ "bar"; "bat" ] in
  Ppx_sexp_conv_lib.Sexp.List
    (Ppx_sexp_conv_lib.Sexp.List
       [ Ppx_sexp_conv_lib.Sexp.Atom "head"; Ppx_sexp_conv_lib.Conv.sexp_of_string "foo" ]
     ::
     (match tail, [] with
      | None, tl -> tl
      | Some v, tl ->
        Ppx_sexp_conv_lib.Sexp.List
          [ Ppx_sexp_conv_lib.Sexp.Atom "tail"
          ; (fun [@merlin.hide] x__003_ -> sexp_of_list sexp_of_string x__003_) v
          ]
        :: tl))
;;

let _ =
  let b = "b" in
  Ppx_sexp_conv_lib.Sexp.Atom
    (Ppx_string_runtime.For_string.concat
       [ Ppx_string_runtime.For_string.of_string "a"
       ; b
       ; Ppx_string_runtime.For_string.of_string "c"
       ] [@merlin.hide] [@nontail])
;;

let _ =
  let side_effect = ref false in
  (sexp_of_unit [@merlin.hide]) (side_effect := true)
;;

[@@@end]
