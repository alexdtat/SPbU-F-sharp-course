module h1.ReverseList

let reverse list =
    let rec recReverse acc = function
        | [] -> acc
        | head :: tail -> recReverse (head :: acc) tail
    recReverse [] list
    
printfn "%A" <| reverse [0..5]
