module D02

open System.IO

let rec calc (opcodes: int []) idx =
    match opcodes.[idx] with
    | 1 ->
        let sum =
            opcodes.[opcodes.[idx + 1]]
            + opcodes.[opcodes.[idx + 2]]

        opcodes.[opcodes.[idx + 3]] <- sum
        calc opcodes (idx + 4)
    | 2 ->
        let product =
            opcodes.[opcodes.[idx + 1]]
            * opcodes.[opcodes.[idx + 2]]

        opcodes.[opcodes.[idx + 3]] <- product
        calc opcodes (idx + 4)
    | 99 -> opcodes.[0]
    | _ -> raise (System.ArgumentException("Wrong opcode"))


let findPair (opcodes: int list) =
    let mutable noun = 0
    let mutable verb = 0
    for i in 0 .. 99 do
        for j in 0 .. 99 do
            let codes = opcodes |> Seq.toArray
            codes.[1] <- i
            codes.[2] <- j
            match calc codes 0 with
            | 19690720 ->
                noun <- i
                verb <- j
            | _ -> ()
    (noun, verb)


let run path =
    path
    |> File.ReadAllText
    |> fun x -> x.Split ','
    |> Array.map int
    |> Seq.toList
    |> findPair
    |> fun (n, v) -> 100 * n + v
