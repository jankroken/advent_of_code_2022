module advent_of_code_2022.Day3

open advent_of_code_2022.Common

let parse (s:string) =
    let s = s.ToCharArray ()
    let comp1 = s |> Array.take (s.Length/2) |> Set
    let comp2 = s |> Array.skip (s.Length/2) |> Set
    [comp1;comp2]

let input = example 3 |> List.map parse

let toPriority (c:char) =
    if c < 'a' then (c - 'A' |> int) + 27
    else (c - 'a' |> int) + 1 
let task1 () =
    let prioritySum =
        input
        |> List.toSeq 
        |> Seq.map Set.intersectMany
        |> Seq.collect Set.toSeq
        |> Seq.map toPriority 
        |> Seq.sum
    printfn $"Day 3: Answer 1: {prioritySum}"

let task2 () =
    let prioritySum = 
        input
        |> List.map Set.unionMany 
        |> List.chunkBySize 3
        |> List.map Set.intersectMany
        |> List.map Set.minElement
        |> List.map toPriority
        |> List.sum
    printfn $"Day 3: Answer 2: {prioritySum}"
    

let solution () =
    task1 ()
    task2 ()  