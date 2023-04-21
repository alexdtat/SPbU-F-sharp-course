module hw2.evens

let countEvensFilter list =
    list |> List.filter (fun elem -> elem % 2 = 0) |> List.length

let countEvensFold list =
    list
    |> List.fold (fun acc elem ->
        acc
        + match elem with
          | _ when elem % 2 = 0 -> 1
          | _ -> 0) 0

let countEvensMap list =
    list
    |> List.map (fun elem ->
        match elem with
        | _ when elem % 2 = 0 -> 1
        | _ -> 0)
    |> List.sum
