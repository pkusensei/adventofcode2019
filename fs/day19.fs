module D19

open System.IO

let getCodes path =
    File.ReadAllText path
    |> fun x -> x.Split ','
    |> Array.map (fun x -> x.Trim() |> int64)

let checkPoint x y (computer: D11.IntcodeComputer) =
    computer.InputAndRun(x)
    computer.InputAndRun(y)
    computer.OutputQueue.Dequeue() = 1L

let countPoints codes =
    let mutable res = 0
    for x = 0 to 49 do
        for y = 0 to 49 do
            if checkPoint x y (D11.IntcodeComputer(codes))
            then res <- res + 1
    res

let run1 path = getCodes path |> countPoints
// 186

let fitPoint points (x, y) =
    Set.contains (x, y) points
    && Set.contains (x + 100 - 1, y) points
    && Set.contains (x, y + 100 - 1) points

let scan codes =
    let mutable points = Set.empty
    for x = 1 to 1500 do
        for y = 1 to 1500 do
            if checkPoint x y (D11.IntcodeComputer(codes))
            then points <- Set.add (x, y) points
    points

let run2 path =
    let points = getCodes path |> scan
    points
    |> Seq.find (fun x -> fitPoint points x)
    |> fun (x, y) -> 10000 * x + y
// 9231141