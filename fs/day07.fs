module D07

open System
open System.IO
open System.Collections.Generic

let mutable signalTaken = false
let mutable signalInput = 0
let mutable ampInput = 0

let param (opcodes: int []) idx paramId =
    let inst = opcodes.[idx]

    let mode =
        match paramId with
        | 1 -> ((inst % 10000) % 1000) / 100
        | 2 -> (inst % 10000) / 1000
        | 3 -> inst / 10000
        | _ -> raise (ArgumentException("Wrong parameter id"))

    match mode with
    | 0 -> opcodes.[idx + paramId]
    | 1 -> idx + paramId
    | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))


let rec calc (opcodes: int []) idx =
    let inst = opcodes.[idx]

    match inst % 100 with
    | 1 ->
        let sum =
            opcodes.[param opcodes idx 1]
            + opcodes.[param opcodes idx 2]

        opcodes.[param opcodes idx 3] <- sum
        calc opcodes (idx + 4)
    | 2 ->
        let product =
            opcodes.[param opcodes idx 1]
            * opcodes.[param opcodes idx 2]

        opcodes.[param opcodes idx 3] <- product
        calc opcodes (idx + 4)
    | 3 ->
        opcodes.[param opcodes idx 1] <- if not signalTaken then signalInput else ampInput
        signalTaken <- (not signalTaken)
        calc opcodes (idx + 2)
    | 4 ->
        ampInput <- opcodes.[param opcodes idx 1]
        calc opcodes (idx + 2)
    | 5 ->
        match opcodes.[param opcodes idx 1] with
        | 0 -> calc opcodes (idx + 3)
        | _ -> calc opcodes opcodes.[param opcodes idx 2]
    | 6 ->
        match opcodes.[param opcodes idx 1] with
        | 0 -> calc opcodes opcodes.[param opcodes idx 2]
        | _ -> calc opcodes (idx + 3)
    | 7 ->
        opcodes.[param opcodes idx 3] <- if opcodes.[param opcodes idx 1] < opcodes.[param opcodes idx 2]
                                         then 1
                                         else 0
        calc opcodes (idx + 4)
    | 8 ->
        opcodes.[param opcodes idx 3] <- if opcodes.[param opcodes idx 1] = opcodes.[param opcodes idx 2]
                                         then 1
                                         else 0
        calc opcodes (idx + 4)

    | 99 -> opcodes.[0]

    | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))

let signals =
    seq {
        for i1 in 0 .. 4 do
            for i2 in 0 .. 4 do
                for i3 in 0 .. 4 do
                    for i4 in 0 .. 4 do
                        for i5 in 0 .. 4 -> [| i1; i2; i3; i4; i5 |]
    }
    |> Seq.filter (fun signal -> (Set.ofArray signal).Count = 5)


let runAmp (opcodes: int []) (signal: int []) =
    ampInput <- 0

    for i in 0 .. 4 do
        signalInput <- signal.[i]
        calc (Array.copy opcodes) 0 |> ignore

    (ampInput, signal)

let findMaxSignal (opcodes: int []) (signals: seq<int []>) =
    signals
    |> Seq.map (runAmp (Array.copy opcodes))
    |> Seq.maxBy fst

let run path =
    path
    |> File.ReadLines
    |> Seq.head
    |> fun x -> x.Split ',' |> Array.map int
    |> fun codes -> findMaxSignal codes signals


let test1 =
    "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> runAmp codes [| 4; 3; 2; 1; 0 |]

let test2 =
    "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> findMaxSignal codes signals

let test3 =
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> runAmp codes [| 0; 1; 2; 3; 4 |]

let test4 =
    "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> findMaxSignal codes signals

let test5 =
    "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> findMaxSignal codes signals


type Amp(opcodes: int [], inputQueue: Queue<int>, outputQueue: Queue<int>) =
    let opcodes = opcodes
    let inputQueue = inputQueue
    let outputQueue = outputQueue
    let mutable complete = false
    let mutable paused = false
    let mutable output = 0
    let mutable ip = 0

    let rec operate idx =
        let inst = opcodes.[idx]

        match inst % 100 with
        | 1 ->
            let sum =
                opcodes.[param opcodes idx 1]
                + opcodes.[param opcodes idx 2]

            opcodes.[param opcodes idx 3] <- sum
            operate (idx + 4)
        | 2 ->
            let product =
                opcodes.[param opcodes idx 1]
                * opcodes.[param opcodes idx 2]

            opcodes.[param opcodes idx 3] <- product
            operate (idx + 4)
        | 3 ->
            if inputQueue.Count <> 0 then
                opcodes.[param opcodes idx 1] <- inputQueue.Dequeue()
                operate (idx + 2)
            else
                paused <- true
                ip <- idx
        | 4 ->
            output <- opcodes.[param opcodes idx 1]
            outputQueue.Enqueue output
            paused <- true
            ip <- idx + 2
            operate (idx + 2)
        | 5 ->
            match opcodes.[param opcodes idx 1] with
            | 0 -> operate (idx + 3)
            | _ -> operate opcodes.[param opcodes idx 2]
        | 6 ->
            match opcodes.[param opcodes idx 1] with
            | 0 -> operate opcodes.[param opcodes idx 2]
            | _ -> operate (idx + 3)
        | 7 ->
            opcodes.[param opcodes idx 3] <- if opcodes.[param opcodes idx 1] < opcodes.[param opcodes idx 2]
                                             then 1
                                             else 0
            operate (idx + 4)
        | 8 ->
            opcodes.[param opcodes idx 3] <- if opcodes.[param opcodes idx 1] = opcodes.[param opcodes idx 2]
                                             then 1
                                             else 0
            operate (idx + 4)

        | 99 -> complete <- true

        | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))

    member this.Run() =
        paused <- false
        if (not complete) && (not paused) then operate ip |> ignore

    member this.Complete = complete
    member this.Output = output

let fbSignals =
    seq {
        for i1 in 5 .. 9 do
            for i2 in 5 .. 9 do
                for i3 in 5 .. 9 do
                    for i4 in 5 .. 9 do
                        for i5 in 5 .. 9 -> [| i1; i2; i3; i4; i5 |]
    }
    |> Seq.filter (fun signal -> (Set.ofArray signal).Count = 5)


let runFbAmp (opcodes: int []) (signal: int []) =

    let anyComplete (amps: seq<Amp>) =
        amps
        |> Seq.map (fun x -> x.Complete)
        |> Seq.fold (||) false

    let queues = [| for i in 0 .. 4 -> Queue<int>() |]

    let ampA =
        Amp((Array.copy opcodes), queues.[0], queues.[1])

    let ampB =
        Amp((Array.copy opcodes), queues.[1], queues.[2])

    let ampC =
        Amp((Array.copy opcodes), queues.[2], queues.[3])

    let ampD =
        Amp((Array.copy opcodes), queues.[3], queues.[4])

    let ampE =
        Amp((Array.copy opcodes), queues.[4], queues.[0])

    for i in 0 .. 4 do
        queues.[i].Enqueue signal.[i]
    queues.[0].Enqueue 0

    while (anyComplete [ ampA
                         ampB
                         ampC
                         ampD
                         ampE ]
           |> not) do
        ampA.Run()
        ampB.Run()
        ampC.Run()
        ampD.Run()
        ampE.Run()
    (ampE.Output, signal)


let findMaxFbSignal (opcodes: int []) (signals: seq<int []>) =
    signals
    |> Seq.map (runFbAmp (Array.copy opcodes))
    |> Seq.maxBy fst

let runFb path =
    path
    |> File.ReadLines
    |> Seq.head
    |> fun x -> x.Split ',' |> Array.map int
    |> fun codes -> findMaxFbSignal codes fbSignals


let test6 =
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> runFbAmp codes [| 9; 8; 7; 6; 5 |]


let test7 =
    "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> findMaxFbSignal codes fbSignals


let test8 =
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> runFbAmp codes [| 9; 7; 8; 5; 6 |]

let test9 =
    "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> findMaxFbSignal codes fbSignals
