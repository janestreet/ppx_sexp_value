open Core
module Unix = Core_unix
module Time = Time_float_unix

(* Examples from ../README.md *)

let _basic_use = [%sexp { a = "hello"; b = (Time.now () : Time.t) }]

let _expressions_with_their_evaluations ~x ~y ~z =
  [%sexp ~~(x : int), (y + z : int), "literal"]
;;

let _recomended_use_for_errors ~tmpfile ~dst =
  try Unix.rename ~src:tmpfile ~dst with
  | exn ->
    raise_s
      [%sexp
        "Error while renaming file"
        , { source = (tmpfile : string); dest = (dst : string); exn : exn }]
;;

let _stack_allocated () = exclave_
  let elems =
    Local_iterators_to_be_replaced.List.init_local 10 ~f:(fun x -> exclave_
      string_of_int x)
  in
  [%sexp__stack (elems : string list)]
;;

(* using ppx template *)

let%template[@alloc a = stack] _stack_allocated () =
  (let elems =
     Local_iterators_to_be_replaced.List.init_local 10 ~f:(fun x -> exclave_
       string_of_int x)
   in
   [%sexp (elems : string list)] [@alloc a])
  [@exclave_if_stack a]
;;
