module hw1.PowersOfTwoList
let rec powersOfTwoList n m =
    let rec fastPower value power result =
        match power with
            | 0 -> result
            | _ when power % 2 = 1 -> fastPower (value * value) (power / 2) (result * value)
            | _ -> fastPower (value * value) (power / 2) result
    let rec recPowersOfTwoList previous n m counter =
        match counter with
            | 0 -> let firstElement = fastPower 2UL n 1UL
                   firstElement :: recPowersOfTwoList firstElement n m 1
            | _ when counter <= m ->
                let newElement = previous * 2UL
                newElement :: recPowersOfTwoList newElement n m (counter + 1)
            | _ -> []
    recPowersOfTwoList 1UL n m 0

printfn "%A" <| powersOfTwoList 0 12
