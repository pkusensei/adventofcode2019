module D01

open System.IO

let getFuel num = num / 3 - 2

let getFuelP2 num =
    let rec inner fuel =
        let f = getFuel fuel
        if f < 0 then 0
        elif f < 3 then f
        else f + (inner f)

    let fuel = getFuel num
    fuel + (inner fuel)

let sumFuel nums = Seq.sumBy getFuelP2 nums

let run path =
    path |> File.ReadLines |> Seq.map int |> sumFuel
