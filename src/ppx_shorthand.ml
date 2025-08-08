open! Stdppx

let () =
  Ppxlib.Driver.register_transformation
    "shorthand"
    ~extensions:
      (List.concat
         [ Eta.extensions; Exclave.extensions; Rederive.extensions; Self.extensions ])
;;
