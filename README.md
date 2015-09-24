A ppx rewriter that simplifies building s-expressions from ocaml values.

Basic use
---------

The building block of this preprocessor is the extension:

```ocaml
[%sexp expr]
```

in expressions. It evaluates to an s-expression. This is done by
recursing down the expression and converting all data constructors
into s-expressions. If an expression with a type annotation is found,
then the type is used to convert to s-expression. Expressions that are
neither data constructors nor annotated with a type will be rejected.

For instance:

```ocaml
[%sexp { a = "hello" ; b = (Time.now () : Time.t) } ]
```

will be preprocessed into:

```ocaml
List [List [Atom "a"; Atom "hello"];
      List [Atom "b"; [%sexp_of: Time.t] (Time.now ())];
     ]
```

This does not require a record with fields a and b to exist (and if
one does exist, its sexp_of function will be ignored unless a type
annotation is added around the record).
One can annotate a record field with the type `sexp_option` to achieve
the same result as when using sexplib.

Variant, polymorphic variants, tuples and lists are supported as
well.  Variants are analogous to records in that a type containing the
variant does not have to exist unless a type annotation is added to
the variant.

Expressions with their evaluations
----------------------------------

The `[%sexp]` extensions also let one prints a list of expressions
with their evaluation. For instance:

```ocaml
[%sexp (x : int) (y + z : int) "literal" ]
```

will be preprocessed into:

```ocaml
List [List [Atom "x";     [%sexp_of: int] x];
      List [Atom "x + y"; [%sexp_of: int] (x + y)];
      [%sexp_of: string] "literal";
     ]
```

This form is only allowed when the contents of a `[%sexp]` is a
sequence of two or more expressions (i.e. syntactically an application
of a function to at least one argument).

Recommended use for errors
--------------------------

This extension is primarily intended to build better errors, by making
building errors easier and more readable, particularly when using
records to add context:

```ocaml
try Unix.rename ~src:tmpfile ~dst
with exn ->
  failwiths "Error while renaming file"
    [%sexp
      { source = (tmpfile : string)
      ; dest   = (dst     : string)
      ; exn    = (exn     : exn   )
      }]
    Fn.id
```

Derived extensions (deprecated)
-------------------------------

Some derived extensions are built on top of the base extension.

```ocaml
[%structural_sexp expr]
```

in an alias for `[%sexp expr]`.

```ocaml
[%structural_error string-expr expr]
```

will build a value of type `Error.t` rather than return a sexp. The
string expression is a message which will appear in the error.

```ocaml
[%raise_structural_sexp string-expr expr]
```

is equivalent to `Error.raise [%structural_error string-expr expr]`.

```ocaml
[%structural_or_error string-expr expr]
```

is equivalent to `Result.Error [%structural_error string-expr expr]`.
