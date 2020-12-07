module D09

open System
open System.Collections.Generic
open System.IO

type IntcodeComputer(opcodes: int64 [], input: int) =
    let mutable opcodes = Array.copy opcodes
    let mutable relativeBase = 0
    let outputQueue = Queue<int64>()
    let inputQueue = Queue<int64>()

    do
        inputQueue.Enqueue (int64 input)
        opcodes <- Array.append opcodes (Array.zeroCreate (6 * opcodes.Length))

    let param idx paramId =
        let inst = opcodes.[idx]

        let mode =
            match paramId with
            | 1 -> ((inst % 10000L) % 1000L) / 100L
            | 2 -> (inst % 10000L) / 1000L
            | 3 -> inst / 10000L
            | _ -> raise (ArgumentException("Wrong parameter id"))

        match mode with
        | 0L -> int opcodes.[idx + paramId]
        | 1L -> idx + paramId
        | 2L -> relativeBase + int opcodes.[idx + paramId]
        | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))

    let rec operate idx =
        let inst = opcodes.[idx]

        match int32 (inst % 100L) with
        | 1 ->
            let sum =
                opcodes.[param idx 1] + opcodes.[param idx 2]

            opcodes.[param idx 3] <- sum
            operate (idx + 4)
        | 2 ->
            let product =
                opcodes.[param idx 1] * opcodes.[param idx 2]

            opcodes.[param idx 3] <- product
            operate (idx + 4)
        | 3 ->
            opcodes.[param idx 1] <- inputQueue.Dequeue()
            operate (idx + 2)
        | 4 ->
            outputQueue.Enqueue opcodes.[param idx 1]
            operate (idx + 2)
        | 5 ->
            match opcodes.[param idx 1] with
            | 0L -> operate (idx + 3)
            | _ -> operate (int32 opcodes.[param idx 2])
        | 6 ->
            match opcodes.[param idx 1] with
            | 0L -> operate (int32 opcodes.[param idx 2])
            | _ -> operate (idx + 3)
        | 7 ->
            opcodes.[param idx 3] <- if opcodes.[param idx 1] < opcodes.[param idx 2]
                                     then 1L
                                     else 0L
            operate (idx + 4)
        | 8 ->
            opcodes.[param idx 3] <- if opcodes.[param idx 1] = opcodes.[param idx 2]
                                     then 1L
                                     else 0L
            operate (idx + 4)
        | 9 ->
            relativeBase <- relativeBase + int32 opcodes.[param idx 1]
            operate (idx + 2)
        | 99 -> opcodes.[0]

        | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))

    member this.OutputQueue = outputQueue
    member this.Run() = operate 0

let test1 =
    "05.txt"
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes, 5)
    |> fun x ->
        x.Run() |> ignore
        assert (x.OutputQueue.Count = 1)
        assert (x.OutputQueue.Dequeue() = 2808771L)


let test2 =
    let input =
        "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        |> fun x -> x.Split ','
        |> Array.map int64

    input
    |> fun codes -> IntcodeComputer(codes, 5)
    |> fun x ->
        x.Run() |> ignore
        assert ((x.OutputQueue |> Array.ofSeq) = input)

let test3 =
    "1102,34915192,34915192,7,4,7,99,0"
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes, 5)
    |> fun x ->
        x.Run() |> ignore
        assert (x.OutputQueue.Count = 1)

        let s =
            x.OutputQueue
            |> Array.ofSeq
            |> Array.map char
            |> String

        assert (s.Length = 16)

let test4 =
    "104,1125899906842624,99"
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes, 5)
    |> fun x ->
        x.Run() |> ignore
        assert (x.OutputQueue.Count = 1)
        assert (x.OutputQueue.Dequeue() = 1125899906842624L)


let run path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes, 2)
    |> fun x ->
        x.Run() |> ignore
        x.OutputQueue
