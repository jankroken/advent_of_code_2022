module advent_of_code_2022.Day10

open System.Text
open advent_of_code_2022.Common
open advent_of_code_2022.Debug

type Inst =
    | NOOP
    | ADDX of int

let parse (s: string) : Inst =
    match s.Split [| ' ' |] with
    | [| "noop" |] -> NOOP
    | [| "addx"; I32 value |] -> ADDX value

let program = example 10 |> List.map parse

let rec execute (x: int) (program: Inst list) =
    match program with
    | [] -> []
    | NOOP :: rest -> x :: execute x rest
    | ADDX value :: rest -> x :: x :: execute (x + value) rest

let xOverTime = execute 1 program

let task1 () =
    let signal (i: int) = i * xOverTime[i - 1]
    let points = (xOverTime.Length - 20) / 40

    let answer1 =
        seq { 0..points } |> Seq.map (fun x -> x * 40 + 20) |> Seq.map signal |> Seq.sum

    printfn $"Day 10: Answer 1: {answer1}"

let task2 () =
    let lit (i: int) =
        let p = xOverTime[i] 
        let i = i % 40
        p > i-2 && p < i+2 

    let pixels = seq { 0 .. xOverTime.Length - 1 } |> Seq.map lit |> Seq.toList
    let toChar (lit: bool) = if lit then "█" else "."
    let lines = pixels |> List.map toChar |> List.chunkBySize 40 |> List.map (String.concat "")
    printfn $"Day 10: Answer 2: "
    lines |> List.map (printfn "  %s")

let solution () =
    task1 ()
    task2 ()
