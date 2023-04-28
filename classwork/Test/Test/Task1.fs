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
    
let twice number = number * 2
let minusTwice number = number * -2
([(*) 2; (*) -2], [1; 2; 3]) ||> supermap |> List.iter (fun el -> printf $"%A{el} ")
