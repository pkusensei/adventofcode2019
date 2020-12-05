module D06

open System.IO

let rec countOneObj obj (allObjs: Map<string, string>) =
    match obj with
    | "COM" -> 0
    | _ -> 1 + (countOneObj allObjs.[obj] allObjs)

let countOrbits (allObjs: Map<string, string>) =
    allObjs
    |> Seq.map (fun p -> countOneObj p.Key allObjs)
    |> Seq.sum

let rec buildRoute obj (allObjs: Map<string, string>) =
    match obj with
    | "COM" -> [ "COM" ] |> Set.ofList
    | _ ->
        let route = buildRoute allObjs.[obj] allObjs
        route.Add obj


let countTransfers allObjs =
    let r1 = buildRoute "YOU" allObjs
    let r2 = buildRoute "SAN" allObjs
    (Set.union r1 r2).Count
    - (Set.intersect r1 r2).Count
    - 2


let run path =
    let objs =
        path
        |> File.ReadLines
        |> Seq.map (fun x ->
            x.Split ')'
            |> fun [| x; y |] -> (y, x))
        |> Map.ofSeq

    objs |> countOrbits |> printfn "No. of orbits: %d"
    countTransfers objs


"COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L".Split ','
|> Seq.map (fun x ->
    x.Split ')'
    |> fun [| x; y |] -> (y, x))
|> Map.ofSeq
|> countOrbits
|> printfn "%d"

"COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN".Split ','
|> Seq.map (fun x ->
    x.Split ')'
    |> fun [| x; y |] -> (y, x))
|> Map.ofSeq
|> countTransfers
|> printfn "%d"
