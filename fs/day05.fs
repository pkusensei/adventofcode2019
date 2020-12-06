module D05

open System
open System.IO

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
        printf "Enter input: "
        let input = Console.ReadLine()
        opcodes.[param opcodes idx 1] <- int input
        calc opcodes (idx + 2)
    | 4 ->
        printfn "Output %d" opcodes.[param opcodes idx 1]
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


let run path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int
    |> fun codes -> calc codes 0
