module hw3Tests

open FsCheck
open FsUnit
open LambdaInterpreter.LambdaInterpreter
open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let YCombinatorTest () =
    let YCombinatorEquality f =
        let YCombinator =
            LambdaAbstraction(
                Variable "f",
                let appTerm =
                    LambdaAbstraction(Variable "x", Application(Var "f", Application(Var "x", Var "x")))

                Application(appTerm, appTerm)
            )

        let newVar = Variable(generateUniqueName (Set.add (Variable "x") (freeVariables f)))
        let alphaConverted = alphaConversion newVar (Application(YCombinator, f))

        (Application(f, betaReduction alphaConverted) = betaReduction

        (
            betaReduction

                alphaConverted
        ))

    Check.QuickThrowOnFailure YCombinatorEquality

[<Test>]
let betaReductionTest1 () =
    let term =
        Application(
            LambdaAbstraction("z", Var "y"),
            let appTerm =
                LambdaAbstraction("z", Application(Var "z", Application(Var "z", Var "z")))

            Application(appTerm, appTerm)
        )

    let expectedTerm = Var "y"
    let expectedFree = Set.singleton (Variable "y")
    let expectedBound = Set.singleton (Variable "z")

    (eval term, expectedTerm) ||> should equal
    (freeVariables term, expectedFree) ||> should equal
    (boundVariables term, expectedBound) ||> should equal

[<Test>]
let betaReductionTest2 () =
    let term = Application(LambdaAbstraction("x", Var "x"), Var "z")

    let expectedTerm = Var "z"
    let expectedFree = Set.singleton (Variable "z")
    let expectedBound = Set.singleton (Variable "x")

    (eval term, expectedTerm) ||> should equal
    (freeVariables term, expectedFree) ||> should equal
    (boundVariables term, expectedBound) ||> should equal

[<Test>]
let betaReductionTest3 () =
    let term = Application(LambdaAbstraction("x", Var "z"), Var "x")

    let expectedTerm = Var "z"
    let expectedFree = Set.singleton(Variable "z").Add(Variable "x")
    let expectedBound = Set.singleton (Variable "x")
    (eval term, expectedTerm) ||> should equal
    (freeVariables term, expectedFree) ||> should equal
    (boundVariables term, expectedBound) ||> should equal

[<Test>]
let etaReductionTest () =
    let term =
        LambdaAbstraction(
            Variable "x",
            Application(LambdaAbstraction(Variable "y", Application(Var "z", Var "y")), Var "x")
        )

    let expectedTerm = Var "z"
    let expectedFree = Set.singleton (Variable "z")
    let expectedBound = Set.singleton(Variable "x").Add(Variable "y")
    (etaReduction term, expectedTerm) ||> should equal
    (freeVariables term, expectedFree) ||> should equal
    (boundVariables term, expectedBound) ||> should equal

[<Test>]
let alphaConversionTest () =
    let converted =
        alphaConversion
            "a"
            (LambdaAbstraction(
                Variable "x",
                Application(LambdaAbstraction(Variable "y", Application(Var "z", Var "y")), Var "x")
            ))

    let expectedTerm =
        LambdaAbstraction(
            Variable "a",
            Application(LambdaAbstraction(Variable "y", Application(Var "z", Var "y")), Var "a")
        )

    let expectedFree = Set.singleton (Variable "z")
    let expectedBound = Set.singleton(Variable "a").Add(Variable "y")

    (converted, expectedTerm) ||> should equal
    (freeVariables converted, expectedFree) ||> should equal
    (boundVariables converted, expectedBound) ||> should equal
