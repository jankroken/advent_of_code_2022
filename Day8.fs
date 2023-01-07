module advent_of_code_2022.Day8

open advent_of_code_2022.Common

let map = example 8 |> XYMap.linesToCharMap |> XYMap.mapValues charToInt

let task1 () =
    let west = map |> XYMap.westColumn
    let east = map |> XYMap.eastColumn
    let north = map |> XYMap.northRow
    let south = map |> XYMap.southRow

    let rec scanInwards (nextOf: XY -> XY) (h: int) (pos: XY) : XY list =
        match map.Map.TryFind pos with
        | None -> []
        | Some (height) when height > h -> pos :: (scanInwards nextOf height (nextOf pos))
        | Some (_) -> scanInwards nextOf h (nextOf pos)

    let west = west |> Seq.collect (scanInwards XYMap.eastOf -1) |> Set.ofSeq
    let east = east |> Seq.collect (scanInwards XYMap.westOf -1) |> Set.ofSeq
    let north = north |> Seq.collect (scanInwards XYMap.southOf -1) |> Set.ofSeq
    let south = south |> Seq.collect (scanInwards XYMap.northOf -1) |> Set.ofSeq
    let visible = [ west; east; north; south ] |> Set.unionMany |> Set.count
    printfn $"Day 8: Answer 1: {visible}"

let task2 () =
    let OUTSIDE: XY = -1, -1
    let lines = [ map |> XYMap.columns; map |> XYMap.rows ] |> List.concat
    let lines = lines |> List.map (List.map (fun xy -> xy, map.Map[xy]))

    let rec scan (horizon: Map<int, XY * int * int>) (line: (XY * int) list) : (XY * int) list list =
        match line with
        | [] ->
            horizon
            |> Map.toList
            |> List.map (fun (_, (xy, before, after)) -> xy, before * after)
            |> fun x -> [ x ]
        | (xy, height) :: trees ->
            let horizon = horizon |> Map.map (fun h (xy, l, r) -> xy, l, r + 1)
            let before = horizon.Keys |> Seq.filter (fun h -> h >= height) |> Seq.min
            let (_, _, view) = horizon[before]

            let blocked =
                horizon
                |> Map.toSeq
                |> Seq.filter (fun (h, _) -> h <= height)
                |> Seq.map (fun (_, (xy, before, after)) -> xy, before * after)
                |> Seq.toList

            let horizon = horizon |> Map.filter (fun h _ -> h > height)
            let horizon = horizon.Add(height, (xy, view, 0))
            let trees = scan horizon trees
            blocked :: trees

    let horizon = seq { 1000, (OUTSIDE, 0, -1) } |> Map.ofSeq
    let xyviews = lines |> List.collect (scan horizon)

    let (location,answer2) =
        xyviews
        |> List.concat
        |> List.groupBy fst
        |> List.map (fun (xy, vc) -> xy, (snd vc[0] * snd vc[1]))
        |> List.filter (fun (xy, _) -> xy <> OUTSIDE)
        |> List.maxBy snd
    printfn $"Day 8: Answer2: {answer2}" 

let solution () =
    task1 ()
    task2 ()
