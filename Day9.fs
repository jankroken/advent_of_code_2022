module advent_of_code_2022.Day9

open advent_of_code_2022.Common

let dist = XYMap.distance9X

type Dir =
    | U
    | D
    | L
    | R

type Inst = Dir * int64

type Rope = XY list

let parse (s: string) =
    match s.Split [| ' ' |] with
    | [| "U"; I64 steps |] -> U, steps
    | [| "D"; I64 steps |] -> D, steps
    | [| "L"; I64 steps |] -> L, steps
    | [| "R"; I64 steps |] -> R, steps

let moveHead (inst: Inst) ((x, y)) =
    match inst with
    | D, _ -> (x, y + 1)
    | U, _ -> (x, y - 1)
    | L, _ -> (x - 1, y)
    | R, _ -> (x + 1, y)

let rec moveTail (rope: Rope) =
    match rope with
    | pos1 :: pos2 :: tail when dist pos1 pos2 < 2 -> pos1 :: moveTail (pos2 :: tail)
    | (x1, y1) :: (x2, y2) :: tail ->
        let (dx, dy) = (x1 - x2, y1 - y2) |> unit2D
        let tail = (x2 + dx, y2 + dy) :: tail
        (x1, y1) :: moveTail tail
    | [ e ] -> [ e ]

let takeInst (insts: Inst list) : Inst * Inst list =
    match insts with
    | (_, 1L) :: rest -> insts.Head, insts.Tail
    | (d, n) :: rest -> (d, 1L), (d, n - 1L) :: insts.Tail

let inst = example 9 |> List.map parse

let rec slither ((head :: tail): Rope) (insts: Inst list) : Set<XY> =
    if insts = [] then
        tail[tail.Length-1] |> Set.singleton
    else
        let inst, insts = takeInst insts
        let head = head |> moveHead inst
        let rope = (head :: tail) |> moveTail
        let visited = slither rope insts
        visited.Add (tail[tail.Length-1])

let rope (n: int) = (0, 0) |> List.replicate n

let task1 () =
    let rope = rope 2
    let visited = slither rope inst
    printfn $"Day 9: Answer 1: {visited.Count}"

let task2 () =
    let rope = rope 10
    let visited = slither rope inst
    printfn $"Day 9: Answer 2: {visited.Count}"

let solution () =
    task1 ()
    task2 ()