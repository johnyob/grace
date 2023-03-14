include Core
include Grace

let fail fn =
  match Or_error.try_with fn with
  | Ok _ -> raise_s [%message "Test failed to raise an error."]
  | Error err ->
    Format.printf
      "@[<v>Test raised error as expected.@;Error: @[<hv 7>%a@]@]"
      Error.pp
      err
;;
