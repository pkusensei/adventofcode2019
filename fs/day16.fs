module D16

open System.IO

let basePattern = [| 0; 1; 0; -1 |]

let patternForDigit outputIndex =
    basePattern
    |> Array.collect (fun i -> Array.replicate (outputIndex + 1) i)

let repeatPattern outputLength outputIndex =
    let singlePattern = patternForDigit outputIndex

    outputLength
    / (Array.length singlePattern)
    + 1
    |> fun x -> Array.replicate x singlePattern
    |> Array.concat
    |> Array.tail

let rec operate input times targetTimes =
    let output =
        input
        |> Array.indexed
        |> Array.map (fun (i, _) ->
            let pattern = repeatPattern (Array.length input) i
            input
            |> Array.indexed
            |> Array.sumBy (fun (i, digit) -> pattern.[i] * digit)
            |> fun s -> (s % 10) |> abs)

    if times + 1 < targetTimes then operate output (times + 1) targetTimes else output


let test1 =
    let input =
        "12345678"
        |> Seq.map (string >> int)
        |> Seq.toArray

    operate input 0 4
//01029468

let test2 =
    let input =
        "80871224585914546619083218645595"
        |> Seq.map (string >> int)
        |> Seq.toArray

    operate input 0 100
    |> fun x -> Array.sub x 0 8
//24176176

let test3 =
    let input =
        "19617804207202209144916044189917"
        |> Seq.map (string >> int)
        |> Seq.toArray

    operate input 0 100
    |> fun x -> Array.sub x 0 8
//73745418

let test4 =
    let input =
        "69317163492948606335995924319873"
        |> Seq.map (string >> int)
        |> Seq.toArray

    operate input 0 100
    |> fun x -> Array.sub x 0 8
//52432133

let run1 path =
    path
    |> File.ReadAllText
    |> fun x -> x.Trim()
    |> Seq.map (string >> int)
    |> Array.ofSeq
    |> fun x -> operate x 0 100
    |> fun x -> Array.sub x 0 8
//61149209

let processInput input =
    let inputArray =
        input |> Seq.map (string >> int) |> Seq.toArray

    let skip =
        inputArray
        |> fun x -> Array.sub x 0 7
        |> Array.indexed
        |> Array.sumBy (fun (idx, digit) -> float digit * ((float 10) ** (6.0 - float idx)))
        |> int

    skip, inputArray

let getDigitAt index digits =
    let length = Array.length digits
    Array.get digits (index % length)

let getEndIdx inputArray = 10_000 * (Array.length inputArray) //- 1

let initDigits startIdx endIdx inputArray =
    [| startIdx .. endIdx |]
    |> Array.map (fun i -> getDigitAt i inputArray)

let rec runSteps digits depth totalDepth =
    if depth >= totalDepth then
        Array.sub digits 0 8
    else
        let mutable sum = 0

        let sums =
            digits
            |> Array.indexed
            |> Array.map (fun (i, _) ->
                sum <- sum + digits.[i]
                sum)
            |> fun x -> [ [| 0 |]; x ]
            |> Array.concat

        sums
        |> Array.map (fun v ->
            let value = (Array.last sums) - v
            value % 10)
        |> fun x -> runSteps x (depth + 1) totalDepth

let findSignal input =
    let (skip, inputArray) = processInput input

    let endIdx = getEndIdx inputArray
    initDigits skip endIdx inputArray
    |> fun x -> runSteps x 0 100

let test5 =
    findSignal "03036732577212944063491565474664"
    // 84462026

let test6 =
    findSignal "02935109699940807407585447034323"
    // 78725270

let test7 =
    findSignal "03081770884921959731165446850517"
    // 53553731


let run2 path =
    path
    |> File.ReadAllText
    |> fun x -> x.Trim()
    |> Seq.map (string >> int)
    |> Array.ofSeq
    |> findSignal
