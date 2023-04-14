module hw2.arithmeticTree

module ArithmeticTree =
    type ArithmeticExpression =
        | Number of int
        | Plus of ArithmeticExpression * ArithmeticExpression
        | Multiplication of ArithmeticExpression * ArithmeticExpression
        | Division of ArithmeticExpression * ArithmeticExpression
        | Remainder of ArithmeticExpression * ArithmeticExpression
    let eval arithmeticExpression =
        let rec evalCPS arithmeticExpression continuation =
            match arithmeticExpression with
                | Number num -> continuation num
                | Plus(left, right) -> evalCPS left (fun l -> evalCPS right (fun r -> continuation(l + r)))
                | Multiplication(left, right) -> evalCPS left (fun l -> evalCPS right (fun r -> continuation(l * r)))
                | Division(left, right) -> evalCPS left (fun l -> evalCPS right (fun r -> continuation(l / r)))
                | Remainder(left, right) -> evalCPS left (fun l -> evalCPS right (fun r -> continuation(l % r)))
        evalCPS arithmeticExpression id
