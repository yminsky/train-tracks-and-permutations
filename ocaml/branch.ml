open Core.Std
include Private_int.M

include Pretty_printer.Register (struct
    type z = t
    type t = z
    let module_name = "Branch"
    let to_string = to_string
  end)
