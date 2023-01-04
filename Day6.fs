module advent_of_code_2022.Day6

open advent_of_code_2022.Common
let input = example 6 |> List.head |> fun s -> s.ToCharArray () |> Array.toList  

let task1 () =
   let rec findMarker (input: char list) =
       match input with
       | a::b::c::d::_ when [a;b;c;d] |> Set.ofList |> Set.count = 4 -> 4
       | a::rest -> 1 + findMarker rest
   let answer1 = findMarker input
   printfn $"Day 6: Answer 1: {answer1}"

let task2 () =
    let rec findMarker (input: char list) =
        if input |> List.take 14 |> Set.ofList |> Set.count = 14 then 14
        else 1 + findMarker input.Tail
    let answer2 = findMarker input
    printfn $"Day 7: Answer 2: {answer2}"

let solution () =
    task1 ()
    task2 ()