open Core

(* Examples from ../README.md *)

let _basic_use =
  [%sexp { a = "hello" ; b = (Time.now () : Time.t) } ]

let _expressions_with_their_evaluations ~x ~y ~z =
  [%sexp ~~(x : int), (y + z : int), "literal"]

let _recomended_use_for_errors ~tmpfile ~dst =
  try Unix.rename ~src:tmpfile ~dst
  with exn ->
    raise_s
      [%sexp "Error while renaming file",
             { source = (tmpfile : string)
             ; dest   = (dst     : string)
             ; exn    = (exn     : exn   )
             }]

(* And some extra tests *)
let _poly_variants_and_attributes =
  let module Foo = struct
    type t = [ `A ]
    let sexp_of_t _ = Sexp.Atom "A"
  end in
  let foo = `A in
  let maybe_foo = Some `A in
  [%sexp { foo : Foo.t }],
  [%sexp { foo : [< `A ] }],
  [%sexp { foo : [< Foo.t ] }],
  [%sexp { maybe_foo : Foo.t option }],
  [%sexp { maybe_foo : [< `A ] option }],
  [%sexp { maybe_foo : [< Foo.t ] option }]
