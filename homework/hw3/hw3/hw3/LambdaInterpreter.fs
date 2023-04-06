open System

module LambdaInterpreter =
    type Variable = string

    type LambdaTerm =
        | Var of Variable
        | Application of LambdaTerm * LambdaTerm
        | LambdaAbstraction of Variable * LambdaTerm

    let freeVariables term =
        let rec freeVariablesCPS term continuation =
            match term with
            | Var name -> continuation (Set.singleton name)
            | Application (left, right) ->
                freeVariablesCPS left (fun l -> freeVariablesCPS right (fun r -> continuation (Set.union l r)))
            | LambdaAbstraction (name, term) -> freeVariablesCPS term (fun t -> continuation (Set.remove name t))

        freeVariablesCPS term id

    let boundVariables term =
        let rec boundVariablesCPS term continuation =
            match term with
            | Var _ -> continuation Set.empty
            | Application (left, right) ->
                boundVariablesCPS left (fun l -> boundVariablesCPS right (fun r -> continuation (Set.union l r)))
            | LambdaAbstraction (name, term) -> boundVariablesCPS term (fun t -> continuation (Set.add name t))

        boundVariablesCPS term id

    let generateUniqueName usedVariables =
        let max: string = Set.maxElement usedVariables
        let last = Seq.last max

        match Char.IsDigit last with
        | true ->
            max[0 .. max.Length - 2]
            + (last |> Char.GetNumericValue |> int |> (+) 1 |> string)
        | false -> max[0 .. max.Length - 1] + "0"

    let substitute var term subTerm =
        let needRename lVariable variable term substitutedTerm =
            not (
                Set.contains lVariable (freeVariables substitutedTerm)
                && (Set.contains variable (freeVariables term))
            )

        let rec substituteCPS var term subTerm continuation =
            match term with
            | Var name when name = var -> continuation subTerm
            | Var _ -> continuation term
            | Application (left, right) ->
                substituteCPS var left subTerm (fun l ->
                    substituteCPS var right subTerm (fun r -> continuation (Application(l, r))))
            | LambdaAbstraction (lVar, body) when lVar = var -> continuation (LambdaAbstraction(lVar, body))
            | LambdaAbstraction (lVar, body) when needRename lVar var term subTerm ->
                substituteCPS var body subTerm (fun b -> continuation (LambdaAbstraction(lVar, b)))

            | LambdaAbstraction (lVar, body) ->
                let usedVariables = (freeVariables body, freeVariables term) ||> Set.union
                let newLVar = Variable(generateUniqueName usedVariables)

                substituteCPS lVar body (Var newLVar) (fun t ->
                    substituteCPS newLVar t subTerm (fun b -> continuation (LambdaAbstraction(newLVar, b))))

        substituteCPS var term subTerm id

    let alphaConversion newVariable term =
        match term with
        | Var variable -> Var variable
        | Application (l, r) -> Application(l, r)
        | LambdaAbstraction (lVar, body) when Set.contains newVariable (freeVariables body) ->
            LambdaAbstraction(lVar, body)
        | LambdaAbstraction (lVar, body) -> LambdaAbstraction(newVariable, substitute lVar body (Var newVariable))

    let betaReduction isRecursive term =
        let rec reduce isRecursive term =
            match term with
            | Var variable -> Var variable
            | Application (LambdaAbstraction (lVar, body), substitutedTerm) ->
                match isRecursive with
                | false -> substitute lVar body substitutedTerm
                | true -> reduce isRecursive (substitute lVar body substitutedTerm)
            | Application (l, r) -> Application(l, r)
            | LambdaAbstraction (lVar, body) -> LambdaAbstraction(lVar, body)

        reduce isRecursive term

    let eval term = betaReduction true term

    let etaReduction term =
        let rec reduce term =
            match term with
            | Var variable -> Var variable
            | Application (l, r) -> Application(l, r)
            | LambdaAbstraction (lVar, body) ->
                match body with
                | Application (l, r) when r = Var lVar -> reduce l
                | _ -> LambdaAbstraction(lVar, body)

        reduce term
