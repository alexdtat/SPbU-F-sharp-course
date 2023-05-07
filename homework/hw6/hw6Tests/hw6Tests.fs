module hw6Tests

open FsUnit
open NUnit.Framework
open hw6

[<Test>]
let TestRoundingOneThird () =
    let result =
        Rounding.rounding 3 {
            let! a = 1.0
            let! b = 3.0
            return a / b
        }

    result |> should equal 0.333

[<Test>]
let TestRoundingRound () =
    let result =
        Rounding.rounding 5 {
            let! a = 12.0
            let! b = -4.0
            return a / b
        }

    result |> should equal -3.00000

[<Test>]
let TestInvalidAccuracy () =
    (fun () ->
        Rounding.rounding -1 {
            let! a = 12.0
            let! b = -4.0
            return a / b
        }
        |> ignore)
    |> should throw typeof<System.ArgumentOutOfRangeException>

let calculate = Calculate.StringCalculationsBuilder ()

[<Test>]
let TestStringCalculations () =
    let result =
        calculate {
            let! a = "12"
            let! b = "4"
            let! c = "3"
            return a / b * c
        }

    result |> should equal (Some 9)

[<Test>]
let TestInvalidStringCalculations () =
    let result =
        calculate {
            let! a = "12"
            let! b = "4"
            let! c = "3 Ruined"
            return a / b * c
        }

    result |> should equal None
