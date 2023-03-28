module hw2.primes

let primes () =
    let isPrimeBig n =
        let rec recMultipliers current upperBound =
            match current with
            | _ when current > bigint 1u && current <= upperBound && n % current = bigint 0u -> false
            | _ when current <= upperBound -> recMultipliers (current + bigint 1u) upperBound
            | _ -> true

        (n > bigint 1u)
        && recMultipliers (bigint 2u) ((float >> sqrt >> floor >> bigint) n)
    
    Seq.unfold (fun bigNumber ->
        match bigNumber with
        | _ when isPrimeBig bigNumber && bigNumber = bigint 2u -> Some(bigNumber, bigNumber + bigint 1u)
        | _ when isPrimeBig bigNumber -> Some(bigNumber, bigNumber + bigint 2u)
        | _ -> Some(bigint -1, bigNumber + bigint 1u)) (bigint 2u)
    |> Seq.filter (fun bigNumber -> bigNumber > bigint 0u)
