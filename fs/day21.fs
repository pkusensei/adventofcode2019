module D21

open System
open System.IO

let getCodes path =
    File.ReadAllText path
    |> fun x -> x.Split ','
    |> Array.map (fun x -> x.Trim() |> int64)

let p1Insts =
    [ "NOT C J"
      "AND D J"
      "NOT A T"
      "OR T J"
      "WALK\n" ]
    |> fun x -> String.Join('\n', x)
    |> List.ofSeq

let run path insts =
    let computer = getCodes path |> D11.IntcodeComputer

    insts |> List.iter (int >> computer.InputAndRun)

    while not computer.Complete do
        computer.Run()

    computer.DumpOutput() |> Array.last

let run1 path = run path p1Insts
// 19355790

let p2Insts =
    [ "NOT C J"
      "AND D J"
      "AND H J"
      "NOT B T"
      "AND D T"
      "OR T J"
      "NOT A T"
      "OR T J"
      "RUN\n" ]
    |> fun x -> String.Join('\n', x)
    |> List.ofSeq

let run2 path = run path p2Insts
// 1140920822
