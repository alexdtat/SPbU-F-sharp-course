module TestTask1Tests

open NUnit.Framework
open FsUnit
open Test.Supermap

[<Test>]
let TestSupermap () =
    let expected = [ 2; -2; 4; -4; 6; -6 ]
    ([ (*) 2; (*) -2 ], [ 1; 2; 3 ]) ||> supermap |> should equal expected
