module D08

open System.IO

let getLayers width height data = data |> Seq.chunkBySize (width * height)

let count x = Seq.filter ((=) x) >> Seq.length

let findLayer layers = layers |> Seq.minBy (count '0')

let mul1By2 layer = (count '1' layer) * (count '2' layer)

let checkValid width height =
    getLayers width height >> findLayer >> mul1By2

let decode width height layers =
    let length = width * height
    let mutable image = [| for _ in 1 .. length -> '2' |]

    layers
    |> Seq.iter (fun (layer: char []) ->
        for i in 0 .. length - 1 do
            if image.[i] = '2' then image.[i] <- layer.[i])
    image

let run path =
    path
    |> File.ReadAllText
    |> fun x -> x.Trim()
    |> List.ofSeq
    |> getLayers 25 6
    |> decode 25 6
    |> System.String



let test1 =
    "123456789012"
    |> List.ofSeq
    |> getLayers 3 2
    |> findLayer


let test2 =
    "0222112222120000"
    |> List.ofSeq
    |> getLayers 2 2
    |> decode 2 2
