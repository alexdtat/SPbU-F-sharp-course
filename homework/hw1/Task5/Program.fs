module hw1.FindFirstEntrance

let findFirstEntrance list element =
    let rec recFindFirstEntrance list element counter =
        match list with
        | [] -> None
        | head :: _ when head = element  -> Some(counter)
        | _ -> recFindFirstEntrance list.Tail element (counter + 1)
    recFindFirstEntrance list element 0

printfn $"{findFirstEntrance [0;1;2;3] 3}"
