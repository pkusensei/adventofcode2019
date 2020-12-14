module D17

open System.IO
open System

let getComputer path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ',' |> Array.map int64
    |> D11.IntcodeComputer

let getGraph path =
    getComputer path
    |> fun c ->
        c.Run()
        c.DumpOutput()
    |> Array.map char
    |> String
    |> fun x -> x.Split '\n'
    |> Array.filter (fun x -> x.Trim().Length > 0)
    |> Array.map Array.ofSeq

let isPoundsign (row, col) graph =
    match Array.tryItem row graph with
    | None -> false
    | Some line ->
        match Array.tryItem col line with
        | Some '#' -> true
        | _ -> false

let isIntersection row col graph =
    let res =
        [ (row - 1, col)
          (row + 1, col)
          (row, col - 1)
          (row, col + 1) ]
        |> List.map (fun x -> isPoundsign x graph)
        |> List.reduce (&&)

    match res with
    | true -> Some(row, col)
    | _ -> None


let run1 path =
    let graph = getGraph path
    graph
    |> Array.indexed
    |> Array.map (fun (row, line) ->
        Array.indexed line
        |> Array.map (fun (col, _) -> isIntersection row col graph))
    |> Array.concat
    |> Array.choose id
    |> Array.sumBy (fun (x, y) -> x * y)


type Direction =
    | North
    | South
    | West
    | East

let charToDir ch =
    match ch with
    | '^' -> North
    | 'v' -> South
    | '<' -> West
    | '>' -> East
    | _ -> raise (ArgumentException(sprintf "No such direction %c" ch))

let dirTupleToDir (x, y) =
    match (x, y) with
    | (0, 1) -> South
    | (0, -1) -> North
    | (-1, 0) -> West
    | (1, 0) -> East
    | _ -> raise (ArgumentException(sprintf "No such direction (%d, %d)" x y))

let findStartPosition graph =
    let (row, col) =
        graph
        |> Array.indexed
        |> Array.pick (fun (row, line) ->
            line
            |> Array.indexed
            |> Array.tryPick (fun (col, ch) ->
                match ch with
                | '^'
                | 'v'
                | '<'
                | '>' -> Some((row, col))
                | _ -> None))

    (row, col, charToDir graph.[row].[col])

let findTurn current target =
    match current with
    | North ->
        match target with
        | West -> Some 'L'
        | East -> Some 'R'
        | _ -> None
    | South ->
        match target with
        | West -> Some 'R'
        | East -> Some 'L'
        | _ -> None
    | West ->
        match target with
        | North -> Some 'R'
        | South -> Some 'L'
        | _ -> None
    | East ->
        match target with
        | North -> Some 'L'
        | South -> Some 'R'
        | _ -> None

let findAdjacentPoundsign (row, col) graph positions =
    let opt =
        [ (0, 1); (0, -1); (-1, 0); (1, 0) ]
        |> List.tryFind (fun (x, y) ->
            isPoundsign (row + y, col + x) graph
            && not (Set.contains (row + y, col + x) positions))

    match opt with
    | Some (x, y) -> Some(row + y, col + x, dirTupleToDir (x, y))
    | _ -> None

let moveOneStep (row, col) direction =
    match direction with
    | North -> (row - 1, col)
    | South -> (row + 1, col)
    | West -> (row, col - 1)
    | East -> (row, col + 1)

let rec moveForward (row, col) direction graph step positions =
    let (nextRow, nextCol) = moveOneStep (row, col) direction
    if isPoundsign (nextRow, nextCol) graph then
        let newPositions = Set.add (nextRow, nextCol) positions
        moveForward (nextRow, nextCol) direction graph (step + 1) newPositions
    else
        row, col, step, positions

let rec walk (row, col) currentDir graph positions =
    match findAdjacentPoundsign (row, col) graph positions with
    | None -> []
    | Some (nextRow, nextCol, nextDir) ->

        if Set.contains (nextRow, nextCol) positions then
            []
        else
            let turn = findTurn currentDir nextDir
            match turn with
            | Some t ->
                let (newRow, newCol, steps, newPositions) =
                    moveForward (row, col) nextDir graph 0 positions

                [ string t; steps |> string ]
                @ walk (newRow, newCol) nextDir graph newPositions
            | None -> []


let run2 path =
    let graph = getGraph path
    let (row, col, startDir) = findStartPosition graph
    walk (row, col) startDir graph Set.empty

"
......#########..............................
......#.......#..............................
......#.......#.......................######^
......#.......#.......................#......
......#.......#.......................#......
......#.......#.......................#......
......#####...#...........#############......
..........#...#...........#..................
......###########.........#..................
......#...#...#.#.........#..................
......#...#...#.#.........#..................
......#...#...#.#.........#..................
......#...#...#############..................
......#...#.....#............................
......#######...#............................
..........#.#...#............................
..........#.#...#............................
..........#.#...#............................
..........#######............................
............#................................
............#................................
............#................................
............#...........#######..............
............#...........#.....#..............
........###########.....#.....#..............
........#...#.....#.....#.....#..............
#############.....#.....#.....#...#..........
#.......#.........#.....#.....#...#..........
#.......#.....#############...#...#..........
#.......#.....#...#.....#.#...#...#..........
#.....#############.....#.#...#...#..........
#.....#.#.....#.........#.#...#...#..........
#.....#.#.....#.........###########..........
#.....#.#.....#...........#...#..............
#######.#######...........#...#####..........
..........................#.......#..........
..........................#.......#..........
..........................#.......#..........
..........................#.......#..........
..........................#.......#..........
..........................#########..........
"
|> ignore
