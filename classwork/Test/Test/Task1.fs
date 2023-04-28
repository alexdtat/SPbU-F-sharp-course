module Test.Supermap

let supermap functions list =
    let revFunctions = functions |> List.rev
    let revArguments = list |> List.rev

    let rec iterate list acc =
        match list with
        | [] -> acc
        | h :: t ->
            let rec eval functions acc =
                match functions with
                | [] -> acc
                | fh :: ft -> eval ft ((fh h) :: acc)

            eval revFunctions acc |> iterate t

    iterate revArguments []
