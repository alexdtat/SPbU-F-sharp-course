module hw6.Calculate

open System

type StringCalculationsBuilder() =
    member this.Bind(x: string, f) =
        try
            x |> int |> f
        with :? FormatException ->
            None

    member this.Return(x) = Some x
