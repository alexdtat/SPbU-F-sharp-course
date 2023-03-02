module hw1.FindFirstEntrance

let findFirstEntrance list element =
    let rec recFindFirstEntrance list element counter =
        match list with
        | [] -> None
        | head :: _ when head = element -> Some counter
        | _ :: tail -> recFindFirstEntrance tail element (counter + 1)
    recFindFirstEntrance list element 0

printf $"{findFirstEntrance [0..5] 3}"
