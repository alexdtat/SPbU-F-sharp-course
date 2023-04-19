module hw1.Fibonacci

let fibonacci n = 
    let rec recFibonacci n twoBack oneBack =
        match n with
        | 0u -> 0u
        | 1u -> oneBack
        | _ -> recFibonacci (n - 1u) oneBack (twoBack + oneBack)
    recFibonacci n 0u 1u

printfn $"{fibonacci 8u}"
