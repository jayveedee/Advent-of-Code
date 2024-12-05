module InputHandler

open System.IO

let getRawPuzzleInput (day: string) (useExample: bool): (string list) =
    let dir = Directory.GetCurrentDirectory()
    let dayPath = if useExample then "0" else day
    let path = Path.Combine(dir, "..", "inputs", $"day{dayPath}.txt")
    let fullPath = Path.GetFullPath(path)

    printfn "Getting input from: %s" fullPath
    let lines = File.ReadAllLines(fullPath)

    Array.toList lines

let getFilteredPuzzleInput (day: string) (useExample: bool): (string list list) =
    let lines = getRawPuzzleInput day useExample

    let filteredLines = lines |> List.filter (fun line -> line.Trim() <> "")

    let input = 
        filteredLines
        |> List.map (fun line ->
            line.Split([| ' '; '\t'; '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        )

    input

let getFilteredPuzzleInputT (day: string) (useExample: bool): (string list list) =
    let input = getFilteredPuzzleInput day useExample

    let rec transpose lists =
        match lists with
        | []::_ -> []
        | _ -> List.map List.head lists :: transpose (List.map List.tail lists)

    transpose input