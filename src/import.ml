include struct
  open Ppxlib
  include Ast
  module Ast_builder = Ast_builder.Default

  module Ast_pattern = struct
    include Ast_pattern

    let map3 t ~f = map t ~f:(fun k a b c -> k (f a b c))

    (* [fix f] computes the fixpoint of [f]. *)
    let fix f =
      let rec t ctx loc x k = (to_func (f (of_func t))) ctx loc x k in
      f (of_func t)
    ;;
  end

  module Attribute = Attribute
  module Extension = Extension
  module Location = Location
end

let ghostify =
  object
    inherit Ppxlib.Ast_traverse.map
    method! location loc = { loc with loc_ghost = true }
  end
;;
