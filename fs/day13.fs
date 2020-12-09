module D13

open System.IO

open D11

let getCodes path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ',' |> Array.map int64


let test1 path =
    path
    |> getCodes
    |> fun codes ->
        Array.set codes 0 1L
        codes
    |> IntcodeComputer
    |> fun c ->
        c.Run()
        c.DumpOutput()
    |> Array.map int
    |> Array.chunkBySize 3
    |> Array.countBy (fun [| _; _; v |] -> v = 2)
    |> Array.filter (fst >> (=) true)
    |> Array.exactlyOne
    |> snd
    |> fun count -> assert (count = 258)

let test2 path =
    let computer =
        path
        |> getCodes
        |> fun codes ->
            Array.set codes 0 2L
            codes
        |> IntcodeComputer

    let gameStart () =
        computer.Run()
        computer.DumpOutput()
        |> Array.map int
        |> Array.chunkBySize 3
        |> Array.map (fun [| x; y; v |] -> ((x, y), v))
        |> Map.ofSeq

    let runOneStep input =
        computer.InputAndRun input
        computer.DumpOutput()
        |> Array.map int
        |> Array.chunkBySize 3
        |> Array.map (fun [| x; y; v |] -> ((x, y), v))

    let findObj objId gameStatus =
        gameStatus
        |> Map.toSeq
        |> Seq.find (fun ((_, _), v) -> v = objId)
        |> fun ((x, y), _) -> [| x; y |]

    let findBall = findObj 4
    let findPaddle = findObj 3

    let rec runGame gameStatus =
        let blockCount =
            gameStatus
            |> Map.toSeq
            |> Seq.filter (fun ((x, y), v) -> v = 2)
            |> Seq.length

        match blockCount with
        | 0 -> gameStatus
        | _ ->
            let ball = findBall gameStatus
            let paddle = findPaddle gameStatus

            let input =
                if ball.[0] < paddle.[0] then -1
                elif ball.[0] > paddle.[0] then 1
                else 0

            let output = runOneStep input |> Map.ofArray

            let newStatus =
                Map.fold (fun acc key value -> Map.add key value acc) gameStatus output

            runGame newStatus

    let s =
        gameStart ()
        |> runGame
        |> fun m -> m.[(-1, 0)]

    assert (s = 12765)
