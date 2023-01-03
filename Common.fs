module advent_of_code_2022.Common

open System.IO

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
