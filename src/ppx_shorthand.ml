open! Stdppx

let () =
  Ppxlib.Driver.register_transformation
    "shorthand"
    ~extensions:(List.concat [ Rederive.extensions; Eta.extensions ])
;;
