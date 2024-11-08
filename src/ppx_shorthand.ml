open! Base

let () =
  Ppxlib.Driver.register_transformation
    "shorthand"
    ~extensions:(List.concat [ Rederive.extensions; Eta.extensions ])
;;
