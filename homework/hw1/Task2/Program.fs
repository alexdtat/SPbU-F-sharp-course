module hw1.Fibonacci

let fibonacci n = 
    let rec recFibonacci n twoBack oneBack =
        match n with
        | 0 -> 0u
        | 1 -> oneBack
        | _ -> recFibonacci (n - 1) oneBack (twoBack + oneBack)
    recFibonacci n 0u 1u

printfn $"{fibonacci 8}"
