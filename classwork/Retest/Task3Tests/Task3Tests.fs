module Task3Tests

open NUnit.Framework
open Retest.Moves
open FsUnit

[<Test>]
let TestStandingValid () =
    evalMoves (0, 0) [] |> should equal (Some (0, 0))
    
[<Test>]
let TestLeftNone () =
    evalMoves (0, 0) [Left 5] |> should equal None
    
[<Test>]
let TestMovementValid () =
    evalMoves (0, 0) [Right 5; Top 10] |> should equal (Some (5, 10))

[<Test>]
let TestBottomNone () =
    evalMoves (0, 0) [Right 5; Bottom 10] |> should equal None
