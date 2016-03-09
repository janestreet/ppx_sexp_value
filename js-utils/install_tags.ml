let package_name = "ppx_sexp_value"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_sexp_value", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ppx", Some "../lib/ppx_sexp_value/ppx")
    ],
    [])
  ]
