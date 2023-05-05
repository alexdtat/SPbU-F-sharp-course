module Retest.Moves

type OptionBuilder() =
    member _.Bind(v, f) = Option.bind f v
    member _.Return v = Some v
    member _.Zero() = None

let opt = OptionBuilder()

type movement =
    | Left of int
    | Right of int
    | Top of int
    | Bottom of int

let evalMoves startCoord movements =
    let rec evalMovesRec coord movements =
        let opt = OptionBuilder() // this instance is needed for tests
        let newCoord (x, y) move =
            opt {
                let resCoord =
                    match move with
                    | Left step -> (x - step, y)
                    | Right step -> (x + step, y)
                    | Top step -> (x, y + step)
                    | Bottom step -> (x, y - step)

                if (fst resCoord >= 0 && snd resCoord >= 0) then
                    return resCoord
            }

        opt {
            match movements with
            | [] -> return coord
            | h :: t ->
                let! coord = newCoord coord h
                let! resCoord = evalMovesRec coord t
                return resCoord
        }

    (startCoord, movements) ||> evalMovesRec
