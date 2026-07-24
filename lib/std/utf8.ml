let length s = Format.utf_8_scalar_width ~pos:0 ~len:(String.length s) s
