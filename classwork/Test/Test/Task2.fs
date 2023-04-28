module Test.Rhombus

let rhombus side =
    let rec getLine n =
        match n with
        | _ when n > 2u * side - 2u -> ()
        | _ ->
            let rec getSymbol pos =
                let isStar pos =
                    pos = side - n - 1u
                    || pos = side + n - 1u
                    || (n >= side && pos = n - side + 1u)
                    || (n >= side && pos = 3u * side - 3u - n)

                match pos with
                | _ when pos = 2u * side - 1u ->
                    printf "\n"
                    getLine (n + 1u)
                | _ when isStar pos ->
                    printf "*"
                    getSymbol (pos + 1u)
                | _ ->
                    printf " "
                    getSymbol (pos + 1u)

            getSymbol 0u

    getLine 0u

rhombus 8u
