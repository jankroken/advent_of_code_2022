module advent_of_code_2022.Day4

open advent_of_code_2022.Common

let parse (s:string) =
    let s = s.Split [|',';'-'|]
    (s[0] |> int,s[1] |> int),(s[2] |> int,s[3] |> int)
    
let input = example 4 |> List.map parse

let task1 () =
    let oneCoversAll ((a,b),(c,d)) =
        let cont12 = a <= c && b >= d
        let cont21 = a >= c && b <= d
        cont12 || cont21
    let answer1 = 
        input
        |> List.filter oneCoversAll
        |> List.length
    printfn $"Day 4: Answer 1: {answer1}"

let task2 () =
    let overlaps ((a,b),(c,d)) =
        (a <= c && b >= c) || (a <= d && b >= d)
    let answer2 =
        input
        |> List.filter overlaps
        |> List.length
    printfn $"Day 4: Answer 2: {answer2}"   
     
let solution () =
    task1 ()
    task2 ()
   