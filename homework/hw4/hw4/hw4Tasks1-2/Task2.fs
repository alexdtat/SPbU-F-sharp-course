module hw4.PointFree

let step0 x l = List.map (fun y -> y * x) l
let step1 x = List.map (fun y -> y * x)
let step2 x = List.map (fun y -> (*) y x)
let step3 x = List.map (fun y -> (*) x y) // Multiplication is commutative for ints.
let step4 x = List.map ((*) x)
let step5 x = List.map (x |> (*))
let step6 x = x |> (*) |> List.map
let step7 = (*) >> List.map // Not initialized in FsCheck tests.
let step7Wrapped () = (*) >> List.map
