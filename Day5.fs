module advent_of_code_2022.Day5

open advent_of_code_2022.Common

let parseTowers (lines: string list) : Map<char, char list> =
    let columns (s: string) =
        s.ToCharArray()
        |> Array.chunkBySize 4
        |> Array.map (fun s -> s[1])
        |> Array.toList

    let towerChars =
        lines
        |> List.map columns
        |> List.transpose
        |> List.map (List.skipWhile (fun c -> c = ' '))

    towerChars
    |> List.map (fun l -> l[l.Length - 1], (l |> List.take (l.Length - 1)))
    |> Map.ofList

type Command(count: int, from: char, dest: char) =
    member this.Count = count
    member this.From = from
    member this.Dest = dest
    member this.Noop = count = 0
    member this.Decrease() = Command(count - 1, from, dest)
    override this.ToString() = $"Move {count} from {from} to {dest}"

let parseCommand (s: string) : Command =
    let s = s.Split [| ' ' |]
    Command(s[1][0] - '0' |> int, s[3][0], s[5][0])


let input =
    let [ towers; commands ] = example 5 |> splitByEmptyLine
    let towers = parseTowers towers
    let commands = commands |> List.map parseCommand
    towers, commands

let task1 () =
    let rec exec (towers: Map<char, char list>) (commands: Command list) =
        match commands with
        | [] -> towers
        | c :: rest when c.Noop -> exec towers rest
        | c :: rest ->
            let item = towers[c.From].Head
            let towers = towers.Add(c.From, towers[c.From].Tail)
            let towers = towers.Add(c.Dest, item :: towers[c.Dest])
            let c = c.Decrease()
            exec towers (c :: rest)

    let towers, commands = input
    let towers = exec towers commands

    let answer1 =
        towers.Keys |> Seq.map (fun i -> $"{towers[i][0]}") |> String.concat ""

    printfn $"Day 5: Answer 1: {answer1}"

let task2 () =
    let rec exec (towers: Map<char, char list>) (commands: Command list) =
        match commands with
        | [] -> towers
        | c :: rest when c.Noop -> exec towers rest
        | c :: rest ->
            let item = towers[c.From] |> List.take c.Count
            let towers = towers.Add(c.From, towers[c.From] |> List.skip c.Count)
            let towers = towers.Add(c.Dest, [ item; towers[c.Dest] ] |> List.concat)
            exec towers rest

    let towers, commands = input
    let towers = exec towers commands

    let answer1 =
        towers.Keys |> Seq.map (fun i -> $"{towers[i][0]}") |> String.concat ""

    printfn $"Day 5: Answer 2: {answer1}"

let solution () =
    task1 ()
    task2 ()
