module advent_of_code_2022.Common

open System.IO
open System 

let rec example (day: int) : string list =
    Path.Combine(__SOURCE_DIRECTORY__, $"input/example_{day}.txt")
    |> File.ReadAllLines
    |> Array.toList

let rec splitByEmptyLine (lines: string list) =
    match lines with
    | [] -> [ [] ]
    | t :: rest ->
        match t, splitByEmptyLine rest with
        | "", [] :: rest -> [] :: rest
        | "", rest -> [] :: rest
        | line, [] :: rest -> [ line ] :: rest
        | line, t :: rest -> (line :: t) :: rest

let (|I32|_|) (s:string) =
    match Int32.TryParse s with
    | true,value -> Some(value)
    | false,_ -> None

let (|I64|_|) (s:string) =
    match Int64.TryParse s with
    | true,value -> Some(value)
    | false,_ -> None

let rec exp (i:int) (p:int) =
    if i = 0 then 1
    else i*(exp i (p - 1))