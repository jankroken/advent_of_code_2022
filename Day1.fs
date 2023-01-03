module advent_of_code_2022.Day1

open advent_of_code_2022.Common

let solution () =  
    let numbers =
        example 1
        |> splitByEmptyLine
        |> List.map (List.map int)
        |> List.map List.sum
        |> List.sort
        |> List.rev
    
    let answer1 = numbers.Head
    let answer2 = numbers |> List.take 3 |> List.sum
    
    printfn $"Day 1: Answer 1: {answer1}"
    printfn $"Day 1: Answer 2: {answer2}"

