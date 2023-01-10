module advent_of_code_2022.Common

open System.IO
open System

type XY = int * int

let rec example (day: int) : string list =
    Path.Combine(__SOURCE_DIRECTORY__, $"input/example_{day}.txt")
    |> File.ReadAllLines
    |> Array.toList

let rec input (day: int) : string list =
    Path.Combine(__SOURCE_DIRECTORY__, $"input/input_{day}.txt")
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

let (|I32|_|) (s: string) =
    match Int32.TryParse s with
    | true, value -> Some(value)
    | false, _ -> None

let (|I64|_|) (s: string) =
    match Int64.TryParse s with
    | true, value -> Some(value)
    | false, _ -> None

let numbersInString (s: string) : int64 list =
    let s = s.ToCharArray() |> Array.toList
    let isDigit c = c >= '0' && c <= '9'
    let toDigit c = c - '0' |> int64

    let rec nums (curr: Option<int64>) (chars: char list) : int64 list =
        match curr, chars with
        | Some (n), [] -> [ n ]
        | None, [] -> []
        | None, d :: rest when isDigit d -> nums (Some(toDigit d)) rest
        | Some (n), d :: rest when isDigit d -> nums (Some(n * 10L + (toDigit d))) rest
        | None, _ :: rest -> nums None rest
        | Some (n), _ :: rest -> n :: nums None rest

    nums None s

let firstNumberInString = numbersInString >> List.head 

let charToInt (c: char) = c - '0' |> int
let charToInt64 (c: char) = c - '0' |> int64

let s2chars (s: string) : char list = s.ToCharArray() |> Array.toList

let rec exp (i: int) (p: int) =
    if i = 0 then 1 else i * (exp i (p - 1))

let unit (x: int) =
    if x = 0 then 0
    elif x > 0 then 1
    else - 1

let unit2D ((x, y): XY) : XY = unit x, unit y

exception NotImplemented of string