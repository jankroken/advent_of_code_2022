module advent_of_code_2022.XYMap

open advent_of_code_2022.Common

type XYMap<'A>(map: Map<int * int, 'A>) =
    member this.Map = map

let linesToCharMap (lines: string list) : XYMap<char> =
    let map =
        let toMapEntries (y: int, line: string) =
            line.ToCharArray()
            |> Array.toList
            |> List.indexed
            |> List.map (fun (x, c) -> ((x, y), c))

        lines |> List.indexed |> List.collect toMapEntries |> Map.ofList

    XYMap(map)

let mapValues<'A, 'T> (f: 'A -> 'T) (map: XYMap<'A>) : XYMap<'T> =
    map.Map |> Map.map (fun _ -> f) |> XYMap

let westColumn<'A> (map: XYMap<'A>) : seq<XY> =
    map.Map |> Map.keys |> Seq.filter (fun (x, _) -> x = 0)

let eastColumn<'A> (map: XYMap<'A>) : seq<XY> =
    let maxX, _ = map.Map |> Map.maxKeyValue |> fst
    map.Map |> Map.keys |> Seq.filter (fun (x, _) -> x = maxX)

let northRow<'A> (map: XYMap<'A>) : seq<XY> =
    map.Map |> Map.keys |> Seq.filter (fun (_, y) -> y = 0)

let southRow<'A> (map: XYMap<'A>) : seq<XY> =
    let _, maxY = map.Map |> Map.maxKeyValue |> fst
    map.Map |> Map.keys |> Seq.filter (fun (_, y) -> y = maxY)

let northOf ((x, y): XY) : XY = (x, y - 1)
let southOf ((x, y): XY) : XY = (x, y + 1)
let westOf ((x, y): XY) : XY = (x - 1, y)
let eastOf ((x, y): XY) : XY = (x + 1, y)

let rows<'T> (map: XYMap<'T>) : List<List<XY>> =
    let (_,minY),_ = map.Map |> Map.minKeyValue
    let (_,maxY),_ = map.Map |> Map.maxKeyValue
    let ys = seq { minY .. maxY } |> Seq.toList
    let xys = map.Map.Keys |> Seq.toList 
    ys |> List.map (fun row -> xys |> List.filter (fun (_,y) -> y = row))
    
let columns<'T> (map: XYMap<'T>) : List<List<XY>> =
    let (minX,_),_ = map.Map |> Map.minKeyValue
    let (maxX,_),_ = map.Map |> Map.maxKeyValue
    let xs = seq { minX .. maxX } |> Seq.toList
    let xys = map.Map.Keys |> Seq.toList 
    xs |> List.map (fun col -> xys |> List.filter (fun (x,_) -> x = col))

let distance4X ((x1,y1):XY) ((x2,y2):XY) =
   abs(x1-x2) + abs(y1-y2)

let distance9X ((x1,y1):XY) ((x2,y2):XY) =
    max (abs(x1-x2)) (abs(y1-y2))
