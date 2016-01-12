let package_name = "ppx_sexp_value"

let sections =
  [ ("lib",
    [ ("built_lib_ppx_sexp_value", None)
    ],
    [ ("META", None)
    ])
  ; ("libexec",
    [ ("built_exec_ppx", Some "ppx")
    ],
    [])
  ]
