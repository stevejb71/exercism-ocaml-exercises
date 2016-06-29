let leap_year y =
  let multiple_of n = y mod n = 0
  in multiple_of 4 && (multiple_of 100 |> not || multiple_of 400)
