module hw2Tests

open System
open FsCheck
open FsCheck.FSharp
open FsUnit
open Microsoft.FSharp.Collections
open NUnit.Framework
open hw2.primes
open hw2.evens
open hw2.arithmeticTree.ArithmeticTree
open hw2.treeMap.Tree

[<Test>]
let testCountEvens () =
    let listIsEqualTo list = List.forall ((=) (List.head list)) list

    let implementationsAreEqual testList =
        [ countEvensFilter; countEvensFold; countEvensMap ]
        |> List.map (fun implementation -> implementation testList)
        |> listIsEqualTo

    Check.QuickThrowOnFailure implementationsAreEqual

[<Test>]
let TestArithmeticTreeEval () =
    let plusTest l r =
        eval (Plus(Number(l), Number(r))) = l + r

    let plusCommutativityTest l r =
        eval (Plus(Number(l), Number(r))) = eval (Plus(Number(r), Number(l)))

    let plusAssociativityTest l m r =
        eval (Plus(Number(l), Plus(Number(m), Number(r)))) = eval (Plus(Plus(Number(l), Number(m)), Number(r)))

    let plusNeutralRightTest l =
        eval (Plus(Number(l), Number(0))) = eval (Number(l))

    let plusNeutralLeftTest r =
        eval (Plus(Number(0), Number(r))) = eval (Number(r))

    let multiplicationTest l r =
        eval (Multiplication(Number(l), Number(r))) = l * r

    let multiplicationCommutativityTest l r =
        eval (Multiplication(Number(l), Number(r))) = eval (Multiplication(Number(r), Number(l)))

    let multiplicationAssociativityTest l m r =
        eval (Multiplication(Number(l), Multiplication(Number(m), Number(r)))) = eval (
            Multiplication(Multiplication(Number(l), Number(m)), Number(r))
        )

    let multiplicationNeutralRightTest l =
        eval (Multiplication(Number(l), Number(1))) = eval (Number(l))

    let multiplicationNeutralLeftTest r =
        eval (Multiplication(Number(1), Number(r))) = eval (Number(r))

    let distributivityTest l m r =
        eval (Multiplication(Number(l), Plus(Number(m), Number(r)))) = eval (
            Plus(Multiplication(Number(l), Number(m)), Multiplication(Number(l), Number(r)))
        )

    let divisionTest r l =
        eval (Division(Number(l), Number(r))) = l / r

    let divisionOnOne l =
        eval (Division(Number(l), Number(1))) = eval (Number(l))

    let RemainderTest r l =
        eval (Remainder(Number(l), Number(r))) = l % r

    let RemainderOnOne l =
        eval (Remainder(Number(l), Number(1))) = eval (Number(0))

    Check.QuickThrowOnFailure plusTest
    Check.QuickThrowOnFailure plusAssociativityTest
    Check.QuickThrowOnFailure plusCommutativityTest
    Check.QuickThrowOnFailure plusNeutralLeftTest
    Check.QuickThrowOnFailure plusNeutralRightTest
    Check.QuickThrowOnFailure multiplicationTest
    Check.QuickThrowOnFailure multiplicationAssociativityTest
    Check.QuickThrowOnFailure multiplicationCommutativityTest
    Check.QuickThrowOnFailure multiplicationNeutralLeftTest
    Check.QuickThrowOnFailure multiplicationNeutralRightTest
    Check.QuickThrowOnFailure distributivityTest

    let wrappedNonZeroNumTest test =
        let rnd = Random()

        let nonZero =
            Gen.elements (Seq.initInfinite (fun _ -> rnd.Next()) |> Seq.take 100)
            |> Gen.filter (fun n -> n <> 0)
            |> Arb.fromGen

        Prop.forAll nonZero test

    Check.QuickThrowOnFailure(wrappedNonZeroNumTest divisionTest)
    Check.QuickThrowOnFailure(wrappedNonZeroNumTest RemainderTest)

    Check.QuickThrowOnFailure divisionOnOne
    Check.QuickThrowOnFailure RemainderOnOne




[<Test>]
let testTreeMap () =
    let rec areEqualAfterTransformation inp res transformation =
        match inp, res with
        | Empty, Empty -> true
        | Tree (inpVal, inpL, inpR), Tree (resVal, resL, resR) ->
            transformation inpVal = resVal
            && areEqualAfterTransformation inpL resL transformation
            && areEqualAfterTransformation inpR resR transformation
        | _ -> false

    let testEquality inp transformation =
        areEqualAfterTransformation inp (map transformation inp) transformation

    Check.QuickThrowOnFailure testEquality

[<Test>]
let testPrimes () =
    let primesUpToHundred =
        [ 2
          3
          5
          7
          11
          13
          17
          19
          23
          29
          31
          37
          41
          43
          47
          53
          59
          61
          67
          71
          73
          79
          83
          89
          97 ]
        |> List.map (fun n -> bigint n)
    
    primes () |> Seq.take 25 |> Seq.toList |> should equal primesUpToHundred

    let nthIncrIsPrime n =
        let nthIncr = primes () |> Seq.take ((abs n) + 2) |> Seq.last

        primes ()
        |> Seq.take ((abs n) + 1)
        |> Seq.forall (fun num -> nthIncr % num > bigint 0)

    Check.QuickThrowOnFailure nthIncrIsPrime
