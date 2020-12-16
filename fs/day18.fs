module D18

open System.IO
open System
open System.Collections.Generic

type Coord = { X: int; Y: int }

type Tile =
    | Open
    | Key of uint64
    | Door of uint64

type Graph =
    { Tiles: Map<Coord, Tile>
      Keys: uint
      Start: Coord }

let Directions =
    [ { X = -1; Y = 0 }
      { X = 1; Y = 0 }
      { X = 0; Y = -1 }
      { X = 0; Y = 1 } ]

let getGraph input =
    let mutable coord = { X = 0; Y = 0 }
    let mutable keys = 0u
    let mutable tiles = Map.empty

    input
    |> Seq.indexed
    |> Seq.iter (fun (row, line) ->
        line
        |> Seq.indexed
        |> Seq.iter (fun (col, ch) ->
            let p = { X = col; Y = row }
            match ch with
            | '@' ->
                tiles <- Map.add p Open tiles
                coord <- p
            | '.' -> tiles <- Map.add p Open tiles
            | c when Char.IsUpper c -> tiles <- Map.add p (Door(uint64 ch - uint64 'A')) tiles
            | c when Char.IsLower c ->
                let key = uint64 ch - uint64 'a'
                tiles <- Map.add p (Key(key)) tiles
                keys <- keys ||| (1u <<< int key)
            | _ -> ()))
    { Tiles = tiles
      Keys = keys
      Start = coord }


type State = { MissingKeys: uint; Coord: Coord }

let newState graph =
    { MissingKeys = graph.Keys
      Coord = graph.Start }

let nextState graph state =
    Directions
    |> List.choose (fun { X = dx; Y = dy } ->
        let nextCoord =
            { X = state.Coord.X + dx
              Y = state.Coord.Y + dy }

        match Map.tryFind nextCoord graph.Tiles with
        | Some (Door (key)) ->
            match (state.MissingKeys >>> int key) &&& 1u with
            | 0u ->
                Some
                    { MissingKeys = state.MissingKeys
                      Coord = nextCoord }
            | _ -> None
        | Some (Key (key)) ->
            let mKeys =
                match (state.MissingKeys >>> int key) &&& 1u with
                | 1u -> state.MissingKeys ^^^ (1u <<< int key)
                | _ -> state.MissingKeys

            Some
                { MissingKeys = mKeys
                  Coord = nextCoord }

        | Some (Open) ->
            Some
                { MissingKeys = state.MissingKeys
                  Coord = nextCoord }
        | _ -> None)

let countSteps graph =
    let mutable stepsTo = Map.empty
    let state = newState graph
    let queue = Queue()
    let mutable stepNum = 0
    queue.Enqueue(state)
    stepsTo <- Map.add state 0 stepsTo
    while stepNum = 0 && queue.Count > 0 do
        let state = queue.Dequeue()
        match Map.tryFind state stepsTo with
        | Some (steps) ->
            let currentSteps = steps
            if state.MissingKeys = 0u then stepNum <- currentSteps
            nextState graph state
            |> List.iter (fun ns ->
                if not (Map.containsKey ns stepsTo) then
                    stepsTo <- Map.add ns (currentSteps + 1) stepsTo
                    queue.Enqueue(ns))
        | _ -> ()
    stepNum

let test1 =
    "#########
    #b.A.@.a#
    #########"
    |> fun x -> x.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps

let test2 =
    "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"
    |> fun x -> x.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps
// 86

let test3 =
    "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"
    |> fun x -> x.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps
// 132

let test4 =
    "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"
    |> fun x -> x.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps
// 136

let test5 =
    "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"
    |> fun x -> x.Split '\n'
    |> Array.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps
// 81

let run1 path =
    File.ReadLines path
    |> Seq.map (fun x -> x.Trim())
    |> getGraph
    |> countSteps
// 4762

let rec findKeys tiles seen coord keys =
    let mutable seen = Set.add coord seen

    let mutable found =
        match Map.tryFind coord tiles with
        | Some (Key (key)) -> (1u <<< int key)
        | _ -> 0u

    Directions
    |> List.iter (fun { X = dx; Y = dy } ->
        let nextCoord = { X = coord.X + dx; Y = coord.Y + dy }
        if not (Set.contains nextCoord seen)
           && (Map.containsKey nextCoord tiles) then
            found <- found ||| findKeys tiles seen nextCoord keys)
    keys ||| found

let partition graph =
    let { X = sx; Y = sy } = graph.Start
    let mutable tiles = Map.remove graph.Start graph.Tiles

    Directions
    |> List.iter (fun { X = dx; Y = dy } -> tiles <- Map.remove { X = sx + dx; Y = sy + dy } tiles)

    let corners = [ (-1, -1); (1, 1); (-1, 1); (1, -1) ]

    corners
    |> List.map (fun (dx, dy) ->
        let coord = { X = sx + dx; Y = sy + dy }
        tiles <- Map.add coord Open tiles

        { Tiles = tiles
          Keys = findKeys tiles Set.empty coord 0u
          Start = coord })


// let test6 =
//     "#############
// #DcBa.#.GhKl#
// #.###@#@#I###
// #e#d#####j#k#
// ###C#@#@###J#
// #fEbA.#.FgHi#
// #############"
//     |> fun x -> x.Split '\n'
//     |> Array.map (fun x -> x.Trim())
//     |> getGraph
//     |> partition
//     |> List.sumBy countSteps
// // 32

// let test7 =
//     "#############
// #g#f.D#..h#l#
// #F###e#E###.#
// #dCba@#@BcIJ#
// #############
// #nK.L@#@G...#
// #M###N#H###.#
// #o#m..#i#jk.#
// #############"
//     |> fun x -> x.Split '\n'
//     |> Array.map (fun x -> x.Trim())
//     |> getGraph
//     |> partition
//     |> List.sumBy countSteps
// // 72

let run2 path =
    File.ReadLines path
    |> Seq.map (fun x -> x.Trim())
    |> getGraph
    |> partition
    |> List.sumBy countSteps
// 1876
