module advent_of_code_2022.Day2

open advent_of_code_2022.Common

let parse (line: string) =
    let line = line.ToCharArray()
    line[0], line[2]

let rounds = example 2 |> List.map parse

let task1 () =
    let scores (c: char, m: char) =
        let c = c - 'A' |> int
        let m = m - 'X' |> int
        let i: int = (c - m + 3) % 3
        let adj = (i >>> 1) - (i &&& 1)
        1 + m + 3 * (1 + adj)

    let answer1 = rounds |> List.map scores |> List.sum 
    printfn $"Day 2: Answer 1: {answer1}"

let task2 () =
    let scores (c: char, m: char) =
        let c = c - 'A' |> int
        let m = m |> int &&& 3 
        let hand = (c + 2 + m )  % 3
        hand + 1 + (3*m)
    let answer2 = rounds |> List.map scores
    printfn $"Day 2: Answer 2: {answer2}"
    

let solution () =
    task1 ()
    task2 () 
