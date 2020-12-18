module D22

open System
open System.IO

type Card = { Id: int64; Pos: int64 }

let modulo number modulus = ((number % modulus) + modulus) % modulus

let getCards limit =
    [ for i in 0L .. limit -> { Id = i; Pos = i } ]

type Technique =
    | Reverse
    | Cut of int64
    | Increment of int64

let getTechnique (input: string) =
    let input = input.Trim()
    if input.StartsWith "deal into new stack"
    then Reverse
    elif input.StartsWith "cut"
    then Cut(input.Substring 4 |> int64)
    elif input.StartsWith "deal with increment"
    then Increment(input.Substring 20 |> int64)
    else raise (ArgumentException(sprintf "Invalid input %s" input))

let apply technique length card =
    match technique with
    | Reverse ->
        { Id = card.Id
          Pos = length - card.Pos - 1L }
    | Cut num ->
        { Id = card.Id
          Pos = modulo (card.Pos + length - num) length }
    | Increment num ->
        { Id = card.Id
          Pos = modulo (card.Pos * num) length }

let rec applyToOne card length techniques =
    match techniques with
    | head :: tail ->
        let newCard = apply head length card
        applyToOne newCard length tail
    | [] -> card

let applyToAll cards technique =
    cards
    |> List.map (apply technique (cards |> List.length |> int64))

let rec run cards input =
    match input with
    | head :: tail ->
        let newCards = applyToAll cards head
        run newCards tail
    | [] -> cards

let test1 =
    "deal with increment 7
    deal into new stack
    deal into new stack"
    |> fun x -> x.Split '\n'
    |> Array.map getTechnique
    |> List.ofArray
    |> run (getCards 9L)
    |> List.sortBy (fun { Id = _; Pos = pos } -> pos)
    |> List.map (fun { Id = i; Pos = _ } -> i)

let test2 =
    "cut 6
    deal with increment 7
    deal into new stack"
    |> fun x -> x.Split '\n'
    |> Array.map getTechnique
    |> List.ofArray
    |> run (getCards 9L)
    |> List.sortBy (fun { Id = _; Pos = pos } -> pos)
    |> List.map (fun { Id = i; Pos = _ } -> i)

let test3 =
    "deal with increment 7
    deal with increment 9
    cut -2"
    |> fun x -> x.Split '\n'
    |> Array.map getTechnique
    |> List.ofArray
    |> run (getCards 9L)
    |> List.sortBy (fun { Id = _; Pos = pos } -> pos)
    |> List.map (fun { Id = i; Pos = _ } -> i)

let test4 =
    "deal into new stack
    cut -2
    deal with increment 7
    cut 8
    cut -4
    deal with increment 7
    cut 3
    deal with increment 9
    deal with increment 3
    cut -1"
    |> fun x -> x.Split '\n'
    |> Array.map getTechnique
    |> List.ofArray
    |> run (getCards 9L)
    |> List.sortBy (fun { Id = _; Pos = pos } -> pos)
    |> List.map (fun { Id = i; Pos = _ } -> i)

let run1 path =
    File.ReadLines path
    |> Seq.map getTechnique
    |> List.ofSeq
    |> applyToOne { Id = 2019L; Pos = 2019L } 10007L
// 1234
