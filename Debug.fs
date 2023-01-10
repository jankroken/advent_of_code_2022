module advent_of_code_2022.Debug

let printlist<'T> (tl: 'T list) = tl |> List.map (printfn "%A")

let numListToString (tl: int list) =
    tl |> List.map (fun i -> $"{i}") |> String.concat " "

let numListToString64 (tl: int64 list) =
    tl |> List.map (fun i -> $"{i}") |> String.concat " "
