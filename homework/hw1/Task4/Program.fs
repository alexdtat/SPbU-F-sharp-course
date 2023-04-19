module hw1.PowersOfTwoList
let rec powersOfTwoList n m =
    let rec fastPower value power result =
        match power with
            | 0u -> result
            | _ when power % 2u = 1u -> fastPower (value * value) (power / 2u) (result * value)
            | _ -> fastPower (value * value) (power / 2u) result
    let rec recPowersOfTwoList previous n m counter =
        match counter with
            | 0u -> let firstElement = fastPower 2UL n 1UL
                    firstElement :: recPowersOfTwoList firstElement n m 1u
            | _ when counter <= m ->
                let newElement = previous * 2UL
                newElement :: recPowersOfTwoList newElement n m (counter + 1u)
            | _ -> []
    recPowersOfTwoList 1UL n m 0u

printfn $"%A{powersOfTwoList 0u 12u}"
