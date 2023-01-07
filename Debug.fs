module advent_of_code_2022.Debug

let printlist<'T> (tl:'T list) = tl |> List.map (printfn "%A") 

