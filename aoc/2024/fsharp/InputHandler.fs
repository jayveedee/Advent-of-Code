module InputHandler

open System.IO

let getPuzzleInput (day: string) (useExample: bool): (string list list) =
    let dir = Directory.GetCurrentDirectory()
    let dayPath = if useExample then "0" else day
    let path = Path.Combine(dir, "..", "inputs", $"day{dayPath}.txt")
    let fullPath = Path.GetFullPath(path)

    printfn "Getting input from: %s" fullPath
    let lines = File.ReadAllLines(fullPath)

    let filteredLines = lines |> Array.filter (fun line -> line.Trim() <> "")

    let input = 
        filteredLines
        |> Array.map (fun line ->
            line.Split([| ' '; '\t'; '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        )
        |> Array.toList

    input

let getPuzzleInputT (day: string) (useExample: bool): (string list list) =
    let input = getPuzzleInput day useExample

    let rec transpose lists =
        match lists with
        | []::_ -> []
        | _ -> List.map List.head lists :: transpose (List.map List.tail lists)

    transpose input