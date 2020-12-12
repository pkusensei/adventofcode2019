module D15

open System
open System.IO

type Position = { X: int; Y: int }

type Node =
    | Wall
    | Open
    | Target

type Direction =
    | North
    | South
    | West
    | East

let intToNode num =
    match num with
    | 0 -> Wall
    | 1 -> Open
    | 2 -> Target
    | _ -> raise (ArgumentException(sprintf "Invalid status code %d" num))

let dirToInt direction =
    match direction with
    | North -> 1
    | South -> 2
    | West -> 3
    | East -> 4

let move position direction =
    match direction with
    | North -> { position with Y = position.Y + 1 }
    | South -> { position with Y = position.Y - 1 }
    | West -> { position with X = position.X - 1 }
    | East -> { position with X = position.X + 1 }


let getComputer path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ',' |> Array.map int64
    |> D11.IntcodeComputer

let runOneStep direction (computer: D11.IntcodeComputer) =
    computer.InputAndRun(dirToInt direction)
    computer.OutputQueue.Dequeue() |> int


let rec walk (computer: D11.IntcodeComputer) nodes position direction depth =
    let nextPos = move position direction
    if Map.containsKey nextPos nodes then
        []
    else
        let newNode =
            runOneStep direction computer |> intToNode

        let newNodes = Map.add nextPos newNode nodes
        match newNode with
        | Wall -> [ (position, false, depth + 1) ]
        | Target -> [ (nextPos, true, depth + 1) ]
        | Open ->
            [ walk computer newNodes nextPos North (depth + 1)
              walk computer newNodes nextPos South (depth + 1)
              walk computer newNodes nextPos West (depth + 1)
              walk computer newNodes nextPos East (depth + 1) ]
            |> List.concat



let test1 path =
    let computer = getComputer path
    let start = { X = 0; Y = 0 }
    let nodes = [ (start, Open) ] |> Map.ofList
    [ walk computer nodes start North 0
      walk computer nodes start South 0
      walk computer nodes start West 0
      walk computer nodes start East 0 ]
    |> List.concat
    |> List.filter (fun (_, isTarget, _) -> isTarget)
    |> List.minBy (fun (_, _, depth) -> depth)
// |> fun (_, _, d) -> d
// 220

let test2 path =
    let (start, _, _) = test1 path
    let computer = getComputer path
    let nodes = [ (start, Target) ] |> Map.ofList
    [ walk computer nodes start North 0
      walk computer nodes start South 0
      walk computer nodes start West 0
      walk computer nodes start East 0 ]
    |> List.concat
    |> List.maxBy (fun (_, _, depth) -> depth)
    |> fun (_, _, d) -> d
// 334
