module D14

open System.IO

type Chemical = { Name: string; Quant: int64 }

let mulChemical factor chemical =
    { chemical with
          Quant = chemical.Quant * factor }

type Reaction =
    { Product: Chemical
      Ingredients: Chemical list }

let parseLine (line: string) =
    let [| input; output |] =
        line.Trim()
        |> fun x -> x.Split "=>"

    let chems =
        input.Split ','
        |> Array.map (fun x ->
            x.Trim().Split ' '
            |> fun [| quantity; ch |] -> { Name = ch; Quant = int64 quantity })
        |> Array.toList

    let product =
        output.Trim().Split ' '
        |> fun [| quantity; name |] -> { Name = name; Quant = int64 quantity }

    let reaction =
        { Product = product
          Ingredients = chems }

    product.Name, reaction

let parseLines lines = lines |> Seq.map parseLine |> Map.ofSeq

let read path = path |> File.ReadLines |> parseLines

let tryLeftovers target leftovers =
    match Map.tryFind target.Name leftovers with
    | None -> target.Quant, leftovers
    | Some quantity ->
        if target.Quant <= quantity then
            let newQuantity = quantity - target.Quant
            0L, Map.add target.Name newQuantity leftovers
        else
            let remaining = target.Quant - quantity
            remaining, Map.remove target.Name leftovers

let makeFuel amount reactions =
    let fuel = { Name = "FUEL"; Quant = amount }
    let requirements = [ fuel ]
    let leftovers = Map.empty

    let rec inner requirements oreAmount leftovers =
        match requirements with
        | head :: tail ->
            match head.Name with
            | "ORE" -> head.Quant + inner tail oreAmount leftovers
            | _ ->
                let remaining, leftovers = tryLeftovers head leftovers
                match remaining with
                | 0L -> oreAmount + inner tail oreAmount leftovers
                | _ ->
                    let reaction = Map.find head.Name reactions

                    let factor =
                        (ceil >> int64) (float remaining / float reaction.Product.Quant)

                    let result =
                        reaction.Product.Quant * factor - remaining

                    let newLeftovers =
                        match result > 0L with
                        | false -> leftovers
                        | true ->
                            leftovers
                            |> Map.change head.Name (fun opt ->
                                   match opt with
                                   | None -> Some result
                                   | Some v -> Some(v + result))


                    let newRequirements =
                        reaction.Ingredients
                        |> List.map (mulChemical factor)
                        |> List.append tail

                    oreAmount
                    + inner newRequirements oreAmount newLeftovers
        | _ -> oreAmount

    inner requirements 0L leftovers

let maxFuel amount reactions =
    let unitCost = makeFuel 1L reactions
    let left = amount / unitCost
    let right = 2L * left

    let rec inner left right =
        if right - 1L > left then
            let num = (right - left) / 2L + left
            let cost = makeFuel num reactions
            if cost < amount then inner num right else inner left num
        else
            left

    inner left right


let test1 =
    "10 ORE => 10 A
    1 ORE => 1 B
    7 A, 1 B => 1 C
    7 A, 1 C => 1 D
    7 A, 1 D => 1 E
    7 A, 1 E => 1 FUEL".Split '\n'
    |> parseLines
    |> makeFuel 1L
    |> (=) 31L

let test2 =
    "9 ORE => 2 A
    8 ORE => 3 B
    7 ORE => 5 C
    3 A, 4 B => 1 AB
    5 B, 7 C => 1 BC
    4 C, 1 A => 1 CA
    2 AB, 3 BC, 4 CA => 1 FUEL".Split '\n'
    |> parseLines
    |> makeFuel 1L
    |> (=) 165L

let test3 path = path |> read |> makeFuel 1L // 374457L

let test4 =
    "157 ORE => 5 NZVS
    165 ORE => 6 DCFZ
    44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
    12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
    179 ORE => 7 PSHF
    177 ORE => 5 HKGWZ
    7 DCFZ, 7 PSHF => 2 XJWVT
    165 ORE => 2 GPVTF
    3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT".Split '\n'
    |> parseLines
    |> maxFuel 1000000000000L
    |> (=) 82892753L

let test5 =
    "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
    17 NVRVD, 3 JNWZP => 8 VPVL
    53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
    22 VJHF, 37 MNCFX => 5 FWMGM
    139 ORE => 4 NVRVD
    144 ORE => 7 JNWZP
    5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
    5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
    145 ORE => 6 MNCFX
    1 NVRVD => 8 CXFTF
    1 VJHF, 6 MNCFX => 4 RFSQX
    176 ORE => 6 VJHF".Split '\n'
    |> parseLines
    |> maxFuel 1000000000000L
    |> (=) 5586022L

let test6 =
    "171 ORE => 8 CNZTR
    7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
    114 ORE => 4 BHXH
    14 VRPVC => 6 BMBT
    6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
    6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
    15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
    13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
    5 BMBT => 4 WPTQ
    189 ORE => 9 KTJDG
    1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
    12 VRPVC, 27 CNZTR => 2 XDBXC
    15 KTJDG, 12 BHXH => 5 XCVML
    3 BHXH, 2 VRPVC => 7 MZWV
    121 ORE => 7 VRPVC
    7 XCVML => 6 RJRHP
    5 BHXH, 4 VRPVC => 5 LTCX".Split '\n'
    |> parseLines
    |> maxFuel 1000000000000L
    |> (=) 460664L

let test7 path = path |> read |> maxFuel 1000000000000L // 3568888L
