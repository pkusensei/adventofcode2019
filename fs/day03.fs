module D03

open System.IO

type Coord = { X: int; Y: int }

let distance coord = (abs coord.X) + (abs coord.Y)

let rec last =
    function
    | [ x ] -> x
    | _head :: tail -> last tail
    | _ -> raise (System.ArgumentException("Empty list"))


let findRoute (wire: string) =
    let move coord (step: string) =
        let delta = int step.[1..]
        match step.[0] with
        | 'R' -> seq { for i in coord.X + 1 .. coord.X + delta -> { coord with X = i } }
        | 'L' -> seq { for i in coord.X - 1 .. -1 .. coord.X - delta -> { coord with X = i } }
        | 'U' -> seq { for i in coord.Y + 1 .. coord.Y + delta -> { coord with Y = i } }
        | 'D' -> seq { for i in coord.Y - 1 .. -1 .. coord.Y - delta -> { coord with Y = i } }
        | _ -> raise (System.ArgumentException("Wrong direction"))

    let rec buildRoute current route =
        match route with
        | head :: tail ->
            let next = move current head |> Seq.toList
            // printfn "%d" next.Length
            next @ (buildRoute (last next) tail)
        | [] -> []

    wire.Split ','
    |> Seq.toList
    |> buildRoute { X = 0; Y = 0 }


let findIntersections routes =
    routes
    |> Seq.map Set.ofList
    |> Set.intersectMany
    |> Seq.toList

let run path =
    let routes =
        path |> File.ReadLines |> Seq.map findRoute

    findIntersections routes
    |> Seq.map (fun coord ->
        routes
        |> Seq.sumBy ((List.findIndex ((=) coord) >> (+) 1)))
    |> Seq.min
