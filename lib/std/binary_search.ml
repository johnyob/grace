let find_last_satisfying t ~pred ~get ~length =
  let rec loop ~lo ~hi =
    if lo >= hi
    then lo - 1
    else (
      let mid = lo + ((hi - lo) / 2) in
      if pred (get t mid) then loop ~lo:(mid + 1) ~hi else loop ~lo ~hi:mid)
  in
  let idx = loop ~lo:0 ~hi:(length t) in
  if idx < 0 then None else Some idx
;;
