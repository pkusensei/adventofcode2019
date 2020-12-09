module D11

open System
open System.Collections.Generic
open System.IO

type IntcodeComputer(opcodes: int64 []) =
    let mutable opcodes = Array.copy opcodes
    let mutable relativeBase = 0
    let mutable ip = 0
    let mutable complete = false
    let mutable paused = false
    let outputQueue = Queue<int64>()
    let inputQueue = Queue<int64>()
    do opcodes <- Array.append opcodes (Array.zeroCreate (6 * opcodes.Length))

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
        match paused || complete with
        | true -> ()
        | false ->
            match int (inst % 100L) with
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
                if inputQueue.Count <> 0 then
                    opcodes.[param idx 1] <- inputQueue.Dequeue()
                    operate (idx + 2)
                else
                    paused <- true
                    ip <- idx
            | 4 ->
                outputQueue.Enqueue opcodes.[param idx 1]
                operate (idx + 2)
            | 5 ->
                match opcodes.[param idx 1] with
                | 0L -> operate (idx + 3)
                | _ -> operate (int opcodes.[param idx 2])
            | 6 ->
                match opcodes.[param idx 1] with
                | 0L -> operate (int opcodes.[param idx 2])
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
                relativeBase <- relativeBase + int opcodes.[param idx 1]
                operate (idx + 2)
            | 99 -> complete <- true

            | _ -> raise (ArgumentException(sprintf "Wrong opcode %d" inst))

    member this.OutputQueue = outputQueue
    member this.Complete = complete

    member this.Run() =
        paused <- false
        operate ip

    member this.InputAndRun input =
        inputQueue.Enqueue(int64 input)
        this.Run()

    member this.DumpOutput() =
        let output = outputQueue |> Array.ofSeq
        outputQueue.Clear()
        output

type Direction =
    | Up
    | Down
    | Left
    | Right

let changeDir dir input =
    match input with
    | 0 ->
        match dir with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up
    | 1 ->
        match dir with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
    | _ -> raise (ArgumentException(sprintf "Wrong input %d to change direction" input))

type Robot =
    { X: int
      Y: int
      Dir: Direction
      Brain: IntcodeComputer }

let newRobot opcodes =
    { X = 0
      Y = 0
      Dir = Up
      Brain = IntcodeComputer(opcodes) }

let paint robot panel =
    Map.add (robot.X, robot.Y) (robot.Brain.OutputQueue.Dequeue() |> int) panel

let move robot =
    let dir =
        changeDir robot.Dir (robot.Brain.OutputQueue.Dequeue() |> int)

    let r =
        match dir with
        | Up -> { robot with Y = robot.Y - 1 }
        | Down -> { robot with Y = robot.Y + 1 }
        | Left -> { robot with X = robot.X - 1 }
        | Right -> { robot with X = robot.X + 1 }

    { r with Dir = dir }

let paintMove robot panel =
    let panel = paint robot panel
    let robot = move robot
    robot, panel

let rec runRobot robot panel =
    match robot.Brain.Complete with
    | true -> panel
    | false ->
        let input =
            match Map.tryFind (robot.X, robot.Y) panel with
            | Some value -> value
            | None -> 0

        robot.Brain.InputAndRun input
        let (robot, panel) = paintMove robot panel
        runRobot robot panel

let getOpcodes path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int64

let countPanels path =
    let count =
        path
        |> getOpcodes
        |> newRobot
        |> fun r -> runRobot r Map.empty
        |> Map.count

    assert (count = 1686)

let getCorners panel =
    let coords = panel |> Map.toSeq |> Seq.map fst
    let minX = coords |> Seq.map fst |> Seq.min
    let maxX = coords |> Seq.map fst |> Seq.max
    let minY = coords |> Seq.map snd |> Seq.min
    let maxY = coords |> Seq.map snd |> Seq.max

    minX, minY, maxX, maxY

let getCanvas panel =
    let minX, minY, maxX, maxY = getCorners panel

    let canvas =
        seq {
            for x in 0 .. maxX - minX do
                for y in 0 .. maxY - minY -> ((x, y), 0)
        }
        |> Map.ofSeq

    canvas, (minX, minY, maxX, maxY)

let shiftWhiteDots corners whiteDots =
    let minX, minY, _, _ = corners
    whiteDots
    |> Map.map (fun k v ->
        let x, y = k
        ((x - minX, y - minY), v))


let paintCanvas canvas whiteDots corners =
    let mutable canvas = canvas
    let whiteDots = shiftWhiteDots corners whiteDots
    for KeyValue (k, _) in whiteDots do
        canvas <- Map.add k 1 canvas
    canvas
    |> Map.toArray
    |> Array.groupBy (fun ((_, y), _) -> y)
    |> Array.map (fun line ->
        line
        |> snd
        |> Array.map (fun (_, v) ->
            match v with
            | 0 -> ' '
            | 1 -> '#'
            | _ -> raise (ArgumentException("Value being neither 0 or 1")))
        |> String)


let paintId path =
    let whiteDots =
        path
        |> getOpcodes
        |> newRobot
        |> fun r ->
            r.Brain.InputAndRun 1
            let panel = Map.add (0, 0) 1 Map.empty
            paintMove r panel
        |> fun (r, p) -> runRobot r p
        |> Map.filter (fun _ value -> value = 1)

    let canvas, corners = getCanvas whiteDots
    paintCanvas canvas whiteDots corners
    |> Array.iter (printfn "%s") // GARPKZUL


let testIntcodeComputer =
    "05.txt"
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes)
    |> fun x ->
        x.InputAndRun 5
        assert (x.OutputQueue.Count = 1)
        assert (x.OutputQueue.Dequeue() = 2808771L)

    let input =
        "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        |> fun x -> x.Split ','
        |> Array.map int64

    input
    |> fun codes -> IntcodeComputer(codes)
    |> fun x ->
        x.InputAndRun 5
        assert ((x.OutputQueue |> Array.ofSeq) = input)

    "1102,34915192,34915192,7,4,7,99,0"
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes)
    |> fun x ->
        x.InputAndRun 5
        assert (x.OutputQueue.Count = 1)

        let s =
            x.OutputQueue
            |> Array.ofSeq
            |> Array.map char
            |> String

        assert (s.Length = 16)

    "104,1125899906842624,99"
    |> fun x -> x.Split ','
    |> Array.map int64
    |> fun codes -> IntcodeComputer(codes)
    |> fun x ->
        x.InputAndRun 5
        assert (x.OutputQueue.Count = 1)
        assert (x.OutputQueue.Dequeue() = 1125899906842624L)
