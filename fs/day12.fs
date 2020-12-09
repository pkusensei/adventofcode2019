module D12

open System

// Pure functional solution with records and recursion takes ages
// to (never) finish the first part. I caved in. OOP here we go.

type Moon(x, y, z) =
    member val X = x with get, set
    member val Y = y with get, set
    member val Z = z with get, set
    member val Vx = 0 with get, set
    member val Vy = 0 with get, set
    member val Vz = 0 with get, set

    member this.Move() =
        this.X <- this.X + this.Vx
        this.Y <- this.Y + this.Vy
        this.Z <- this.Z + this.Vz

    member this.ChangeVelocity(moons: seq<Moon>) =
        let vx =
            this.Vx
            |> (+)
                (moons
                 |> Seq.filter (fun m -> m.X > this.X)
                 |> Seq.length)
            |> fun x ->
                x
                - (moons
                   |> Seq.filter (fun m -> m.X < this.X)
                   |> Seq.length)

        let vy =
            this.Vy
            |> (+)
                (moons
                 |> Seq.filter (fun m -> m.Y > this.Y)
                 |> Seq.length)
            |> fun x ->
                x
                - (moons
                   |> Seq.filter (fun m -> m.Y < this.Y)
                   |> Seq.length)

        let vz =
            this.Vz
            |> (+)
                (moons
                 |> Seq.filter (fun m -> m.Z > this.Z)
                 |> Seq.length)
            |> fun x ->
                x
                - (moons
                   |> Seq.filter (fun m -> m.Z < this.Z)
                   |> Seq.length)

        this.Vx <- vx
        this.Vy <- vy
        this.Vz <- vz

    member this.Clone() =
        let m = Moon(this.X, this.Y, this.Z)
        m.Vx <- this.Vx
        m.Vy <- this.Vy
        m.Vz <- this.Vz
        m

    member this.TotalEnergy =
        (Math.Abs(this.X)
         + Math.Abs(this.Y)
         + Math.Abs(this.Z))
        * (Math.Abs(this.Vx)
           + Math.Abs(this.Vy)
           + Math.Abs(this.Vz))

let cloneMoons (moons: Moon list) = moons |> List.map (fun m -> m.Clone())

let runOneStep moons =
    let copy = cloneMoons moons
    moons
    |> List.iter (fun m ->
        m.ChangeVelocity copy
        m.Move())
    moons


let runSteps num moons =
    for _ in 1 .. num do
        runOneStep moons |> ignore
    moons

// let test1 =
//     let e =
//         [ Moon(-1, 0, 2)
//           Moon(2, -10, -7)
//           Moon(4, -8, 8)
//           Moon(3, 5, -1) ]
//         |> runSteps 10
//         |> List.sumBy (fun m -> m.TotalEnergy)

//     assert (e = 179)

// let test2 =
//     let e =
//         [ Moon(-8, -10, 0)
//           Moon(5, 5, 10)
//           Moon(2, -7, 3)
//           Moon(9, -8, -3) ]
//         |> runSteps 100
//         |> List.sumBy (fun m -> m.TotalEnergy)

//     assert (e = 1940)

// let test3 =
//     // actual input
//     let e =
//         [ Moon(-1, 7, 3)
//           Moon(12, 2, -13)
//           Moon(14, 18, -8)
//           Moon(17, 4, -4) ]
//         |> runSteps 1000
//         |> List.sumBy (fun m -> m.TotalEnergy)

//     assert (e = 7077)

let dimensions (moons: Moon list) dim =
    match dim with
    | 0 ->
        [ moons.[0].X
          moons.[0].Vx
          moons.[1].X
          moons.[1].Vx
          moons.[2].X
          moons.[2].Vx
          moons.[3].X
          moons.[3].Vx ]
    | 1 ->
        [ moons.[0].Y
          moons.[0].Vy
          moons.[1].Y
          moons.[1].Vy
          moons.[2].Y
          moons.[2].Vy
          moons.[3].Y
          moons.[3].Vy ]
    | 2 ->
        [ moons.[0].Z
          moons.[0].Vz
          moons.[1].Z
          moons.[1].Vz
          moons.[2].Z
          moons.[2].Vz
          moons.[3].Z
          moons.[3].Vz ]

    | _ -> raise (ArgumentException(sprintf "Wrong dimension %d" dim))


// find least common multiple
let rec gcd x y = if y = 0UL then x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let backToSquareOne moons =
    let initState =
        [ dimensions moons 0
          dimensions moons 1
          dimensions moons 2 ]

    let mutable periods = Array.zeroCreate 3
    let mutable cycle = 0UL
    while Array.contains 0UL periods do
        runOneStep moons |> ignore
        cycle <- cycle + 1UL
        for dim in 0 .. 2 do
            if (periods.[dim] = 0UL)
               && (dimensions moons dim) = initState.[dim] then
                Array.set periods dim cycle
    periods |> Array.fold lcm 1UL

let test4 =
    let e =
        [ Moon(-1, 0, 2)
          Moon(2, -10, -7)
          Moon(4, -8, 8)
          Moon(3, 5, -1) ]
        |> backToSquareOne

    assert (e = 2772UL)
    e

let test5 =
    let e =
        [ Moon(-8, -10, 0)
          Moon(5, 5, 10)
          Moon(2, -7, 3)
          Moon(9, -8, -3) ]
        |> backToSquareOne

    assert (e = 4686774924UL)
    e

let test6 =
    let e =
        [ Moon(-1, 7, 3)
          Moon(12, 2, -13)
          Moon(14, 18, -8)
          Moon(17, 4, -4) ]
        |> backToSquareOne
    assert(e = 402951477454512UL)
    e
