module hw1.Factorial

let factorial n = 
  let rec factorial n acc =
    match n with
    | 0UL -> acc
    | _ -> factorial (n - 1UL) (acc * n) 
  factorial n 1UL 
 
printfn $"{factorial 20UL}"
