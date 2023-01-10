module advent_of_code_2022.Day11

open advent_of_code_2022.Common

type Value =
    | Const of int64
    | Old

type Operation = Value * char * Value

type Monkey =
    { id: int64
      items: int64 list
      op: int64 -> int64
      divBy: int64
      ifTrue: int64
      ifFalse: int64 }

let opToFunction (op: Operation) : int64 -> int64 =
    match op with
    | Old, '*', Const c -> fun i -> i * c
    | Const c, '*', Old -> fun i -> i * c
    | Old, '*', Old -> fun i -> i * i
    | Old, '+', Const c -> fun i -> i + c
    | Const c, '+', Old -> fun i -> i + c
    | Old, '+', Old -> fun i -> i + i
    | Old, '-', Const c -> fun i -> i - c
    | Old, '/', Const c -> fun i -> i / c
    | other -> raise (NotImplemented $"Unexpected equation: {other}")

let parseMonkey (lines: string list) : Monkey =
    let toValue s =
        if s = "old" then Old else s |> int64 |> Const

    let id = lines[0] |> firstNumberInString
    let items = lines[1] |> numbersInString
    let eq = lines[ 2 ].Split [| ' ' |]
    let eq = eq[eq.Length - 3 .. eq.Length - 1]
    let eq = (eq[0] |> toValue), (eq[1][0]), (eq[2] |> toValue)
    let eq = opToFunction eq
    let divBy = lines[3] |> firstNumberInString
    let ifTrue = lines[4] |> firstNumberInString
    let ifFalse = lines[5] |> firstNumberInString

    { id = id
      items = items
      op = eq
      divBy = divBy
      ifTrue = ifTrue
      ifFalse = ifFalse }

let monkeys = example 11 |> List.chunkBySize 7 |> List.map parseMonkey

let simulate (iterations: int, worryDecrease: int64 -> int64) : int64 =
    // monkeys |> printlist
    let divisors = monkeys |> List.map (fun m -> m.id, m.divBy) |> Map.ofList
    let ifTrue = monkeys |> List.map (fun m -> m.id, m.ifTrue) |> Map.ofList
    let ifFalse = monkeys |> List.map (fun m -> m.id, m.ifFalse) |> Map.ofList
    let op = monkeys |> List.map (fun m -> m.id, m.op) |> Map.ofList

    let items =
        monkeys
        |> List.collect (fun m -> m.items |> List.map (fun i -> (m.id, i)))
        |> List.groupBy fst
        |> List.map (fun (id, l) -> id, l |> List.map snd)
        |> Map.ofList

    let monkeys = monkeys |> List.map (fun m -> m.id) |> List.sort

    let monkeyRound (monkey: int64) (items: Map<int64, int64 list>) =
        let divBy = divisors[monkey]
        let mine = items[monkey]
        let mine = mine |> List.map (op[monkey])
        let mine = mine |> List.map worryDecrease
        let mine = mine |> List.map (fun m -> m % divBy = 0, m)
        let toTrue = mine |> List.filter fst |> List.map snd
        let toFalse = mine |> List.filter (fun (check, _) -> not check) |> List.map snd
        let t = ifTrue[monkey]
        let f = ifFalse[monkey]
        let items = items.Add(monkey, [])
        let items = items.Add(t, [ items[t]; toTrue ] |> List.concat)
        let items = items.Add(f, [ items[f]; toFalse ] |> List.concat)
        items

    let rec round (monkeys: int64 list) (items: Map<int64, int64 list>) : Map<int64, int64> * Map<int64, int64 list> =
        match monkeys with
        | [] -> Map.empty, items
        | m :: others ->
            let toEval = items[m].Length
            let items = monkeyRound m items
            let evals, items = round others items
            let evals = evals.Add(m, toEval)
            evals, items

    let rec rounds (i: int) (items: Map<int64, int64 list>) =
        if i = 0 then
            monkeys |> List.map (fun m -> m, 0L) |> Map.ofList
        else
            let evals, items = round monkeys items
            let more = rounds (i - 1) items
            monkeys |> List.map (fun m -> m, evals[m] + more[m]) |> Map.ofList

    let counts = rounds iterations items
    let counts = counts.Values |> Seq.sortDescending |> Seq.take 2 |> Seq.toList
    counts[0] * counts[1]

let task1 () =
    let answer1 = simulate (20, (fun i -> i / 3L))
    printfn $"Day 11: Answer 1: {answer1}"

let task2 () =
    let i = monkeys |> List.map (fun m -> m.divBy) |> List.reduce (fun a b -> a * b)
    let worryDecrease (worry: int64) = worry % i

    let answer2 = simulate (10000, worryDecrease)
    printfn $"Day 11: Answer 2: {answer2}"

let solution () =
    task1 ()
    task2 ()
