module D04

let entries =
    seq { 382345 .. 843167 } |> Seq.map string

let crit entry =
    let lst = entry |> Seq.toList
    let sorted = List.sort lst
    let unique = Set.ofList lst

    sorted = lst && unique.Count < lst.Length

let crit2 entry =
    match crit entry with
    | false -> false
    | true ->
        let lst = Seq.toList entry
        lst
        |> List.countBy id
        |> Seq.map (fun (_, b) -> b)
        |> Seq.exists ((=) 2)

let passes =
    entries |> Seq.filter crit2 |> Seq.length
