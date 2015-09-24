open Core.Std

(* Examples from ../README.md *)

let _basic_use =
  [%sexp { a = "hello" ; b = (Time.now () : Time.t) } ]

let _expressions_with_their_evaluations ~x ~y ~z =
  [%sexp (x : int) (y + z : int) "literal"]

let _recomended_use_for_errors ~tmpfile ~dst =
  try Unix.rename ~src:tmpfile ~dst
  with exn ->
    failwiths "Error while renaming file"
      [%sexp
          { source = (tmpfile : string)
          ; dest   = (dst     : string)
          ; exn    = (exn     : exn   )
          }]
      Fn.id

(* Using [ {expr=42} ] as a representative expression suitable for [%sexp] *)

let _deprecated0 =
  [%sexp {expr=42} ]

let _deprecated1 =
  [%structural_sexp {expr=42} ]

let _deprecated2 =
  [%structural_error "string-expr" {expr=42} ]

let _deprecated3 =
  [%raise_structural_sexp "string-expr" {expr=42} ]

let _deprecated4 =
  [%structural_or_error "string-expr" {expr=42} ]
