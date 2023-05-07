module hw6.Rounding

open System

type RoundingBuilder(accuracy: int) =
    member this.Bind(x: float, f) = f (Math.Round(x, accuracy))
    member this.Return(x: float) = Math.Round(x, accuracy)

let rounding = RoundingBuilder
