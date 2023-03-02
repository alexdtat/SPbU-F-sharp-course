module h1.ReverseList

let reverse list =
    let rec recReverse acc = function
        | [] -> acc
        | head :: tail -> recReverse (head :: acc) tail
    recReverse [] list
    
printfn $"{reverse [0; 1; 2; 3; 4; 5]}"
