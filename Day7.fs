module advent_of_code_2022.Day7

open advent_of_code_2022.Common

type CLEntry =
    | CD of string
    | LS
    | DIR of string
    | FILE of int64 * string

type Folder(files: Map<string, int64>, folders: Map<string, Folder>) =
    member this.Files = files
    member this.Folders = folders

    member this.AddFolder(name: string, folder: Folder) =
        Folder(files, folders.Add(name, folder))

    member this.Size() =
        let fileSize = files.Values |> Seq.sum
        let foldersSize = folders.Values |> Seq.map (fun f -> f.Size()) |> Seq.sum
        fileSize + foldersSize

    member this.Flatten() : Folder list =
        let others = folders.Values |> Seq.collect (fun f -> f.Flatten()) |> Seq.toList
        this :: others

    member this.AddFile(name: string, size: int64) = Folder(files.Add(name, size), folders)
    static member empty = Folder(Map.empty, Map.empty)

let parse (line: string) =
    let line = line.Split [| ' ' |]

    match line with
    | [| "$"; "cd"; folder |] -> CD folder
    | [| "$"; "ls" |] -> LS
    | [| "dir"; folder |] -> DIR folder
    | [| I64 size; filename |] -> FILE(size, filename)

let rec mapFileSystem (folder: Folder) (cl: CLEntry list) : Folder * CLEntry list =
    match cl with
    | [] -> folder, []
    | CD ".." :: rest -> folder, rest
    | CD dir :: rest ->
        let sub = folder.Folders.TryFind dir |> Option.defaultValue Folder.empty
        let sub, rest = mapFileSystem sub rest
        let folder = folder.AddFolder(dir, sub)
        mapFileSystem folder rest
    | LS :: rest -> mapFileSystem folder rest
    | DIR dir :: rest ->
        let folder =
            if folder.Folders.ContainsKey dir then
                folder
            else
                folder.AddFolder(dir, Folder.empty)

        mapFileSystem folder rest
    | FILE (size, name) :: rest ->
        let folder = folder.AddFile(name, size)
        mapFileSystem folder rest

let fileSystem, restCommands =
    example 7 |> List.map parse |> mapFileSystem Folder.empty

let task1 () =
    let folders100k = fileSystem.Flatten() |> List.filter (fun f -> f.Size() <= 100000)
    let answer = folders100k |> List.map (fun f -> f.Size()) |> List.sum
    printfn $"Day 7: Answer 1: {answer}"

let task2 () =
    let maxAllowed = 70000000L - 30000000L // total size - available required
    let toDelete = fileSystem.Size() - maxAllowed 
    let folders = fileSystem.Flatten()
    let sizes = folders |> List.map (fun f -> f.Size())
    let sizes = sizes |> List.filter (fun i -> i >= toDelete)
    let answer = sizes |> List.min
    printfn $"Day 7: Answer 2: {answer}"

let solution () =
    task1 ()
    task2 ()
