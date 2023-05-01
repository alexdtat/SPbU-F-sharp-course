module hw4.brackets

// module bracketsParser =
    (* type bracesByType =
        | Braces
        | Curly
        | Square
    type bracesDirection =
        | Opening of bracesByType
        | Closing of bracesByType
    let bracesType symbol =
        match symbol with
        | '(' | ')' -> Braces
        | '{' | '}' -> Curly
        | '[' | ']' -> Square
        | _ -> None *)
    let private isOpening symbol =
        match symbol with
        | '(' -> true
        | '{' -> true
        | '[' -> true
        | _ -> false

    let private matchClosing symbol =
        match symbol with
        | ')' -> Some '('
        | '}' -> Some '{'
        | ']' -> Some '['
        | _ -> None

    let bracketsAreCorrect string =
        let rec recBracketsAreCorrect input brackets =
            match List.isEmpty input with
            | true -> List.isEmpty brackets
            | false ->
                let h = List.head input
                let t = List.tail input
                match isOpening h with
                | true -> recBracketsAreCorrect t (h :: brackets)
                | false ->
                    match matchClosing h with
                    | None -> recBracketsAreCorrect t brackets
                    | Some _ when List.isEmpty brackets -> false
                    | Some bracket ->
                        let bh = List.head brackets
                        let bt = List.tail brackets
                        bh = bracket && recBracketsAreCorrect t bt

        recBracketsAreCorrect (Seq.toList string) List.Empty

// printfn "%A" <| bracketsParser.bracketsAreCorrect "[()]{}{[()()]()}"
