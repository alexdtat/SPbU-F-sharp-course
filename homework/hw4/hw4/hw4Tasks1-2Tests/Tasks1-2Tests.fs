module hw4Tasks1_2Tests

open FsCheck
open NUnit.Framework
open hw4
open hw4.PointFree

[<SetUp>]
let Setup () = ()


[<Test>]
let testPointFree () =
    let equality implementations =
        List.forall ((=) (List.head implementations)) implementations

    let implementationsAreEqual number testList =
        [ step0; step1; step2; step3; step4; step5; step6; step7Wrapped () ]
        |> List.map (fun step -> step number testList)
        |> equality

    Check.QuickThrowOnFailure implementationsAreEqual

[<Test>]
let ``test simple correct`` () =
    Assert.That (brackets.bracketsAreCorrect "()[]{}")

[<Test>]    
let ``test closing before opening`` () =
    Assert.False (brackets.bracketsAreCorrect ")(][}{")

[<Test>]
let ``test not pair`` () =
    Assert.False ((brackets.bracketsAreCorrect "([{") || (brackets.bracketsAreCorrect ")]}"))
    
[<Test>]
let ``test incorrect closing`` () =
    (brackets.bracketsAreCorrect, ["(]"; "(}"; "[)"; "[}"; "{)"; "{]" ]) ||>  List.forall |> Assert.False
    
[<Test>]
let ``test complex correct`` () =
    Assert.True (brackets.bracketsAreCorrect "(([Hello]{FP}){[world]})")
