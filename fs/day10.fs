module D10

open System
open System.IO

let getCoords (lines: char list list) =
    let mutable res = []
    for i in 0 .. ((-) lines.Length 1) do
        for j in 0 .. ((-) lines.[i].Length 1) do
            if lines.[i].[j] = '#' then res <- List.append res [ (j, i) ]
    res


let getRelativeCoord start target =
    let (x1, y1) = start
    let (x2, y2) = target
    (x2 - x1, y2 - y1)


let getRelativeCoords start coords =
    coords
    |> Seq.filter ((<>) start)
    |> Seq.map (getRelativeCoord start)


let getUniqueCount coords =
    coords
    |> Seq.map (fun (x, y) -> Math.Atan2(double y, double x))
    |> Seq.distinct
    |> Seq.length

let countRelativeCoords coords =
    coords
    |> Seq.map (fun c ->
        let count =
            getRelativeCoords c coords |> getUniqueCount

        (c, count))

let getCoordsOnAngle start coords =
    coords
    |> getRelativeCoords start
    |> Seq.map (fun coord ->
        let x, y = coord

        let arctan =
            Math.Atan2(-double y, double x)
            |> (*) (180.0 / Math.PI)

        let angle =
            if 0.0 <= arctan && arctan <= 90.0 then Math.Abs(arctan - 90.0)
            elif arctan < 0.0 then Math.Abs(arctan) + 90.0
            else 450.0 - arctan

        angle, coord)
    |> Seq.groupBy fst
    |> Seq.sortBy fst
    |> Seq.toList

let test1 =
    ".#..#\n.....\n#####\n....#\n...##".Split '\n'
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> countRelativeCoords
    |> Seq.maxBy snd


let test2 =
    "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n".Split
        '\n'
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> countRelativeCoords
    |> Seq.maxBy snd

let test3 =
    "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.".Split
        '\n'
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> countRelativeCoords
    |> Seq.maxBy snd

let test4 =
    ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..".Split
        '\n'
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> countRelativeCoords
    |> Seq.maxBy snd

let test5 =
    ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##".Split
        '\n'
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> countRelativeCoords
    |> Seq.maxBy snd


let makeBet coords =
    let coord =
        coords
        |> countRelativeCoords
        |> Seq.maxBy snd
        |> fst

    let (relX, relY) =
        getCoordsOnAngle coord coords
        |> fun x -> x.[199]
        |> snd
        |> Seq.exactlyOne
        |> snd

    let (x, y) = coord
    (x + relX) * 100 + (y + relY)


let test6 =
    let count =
        ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##".Split
            '\n'
        |> Seq.map Seq.toList
        |> Seq.toList
        |> getCoords
        |> makeBet

    assert (count = 802)

let run path =
    path
    |> File.ReadLines
    |> Seq.map Seq.toList
    |> Seq.toList
    |> getCoords
    |> makeBet
