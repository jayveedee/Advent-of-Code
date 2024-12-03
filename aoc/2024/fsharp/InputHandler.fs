module InputHandler

open System.IO

let getInput (day: string) (useExample: bool) : (int list list) =
    let dir = Directory.GetCurrentDirectory()
    let dayPath = if useExample then "0" else day
    let path = Path.Combine(dir, "..", "inputs", $"day{dayPath}.txt")
    let fullPath = Path.GetFullPath(path)

    printfn "Getting input from: %s" fullPath
    let lines = File.ReadAllLines(fullPath)

    let filteredLines = lines |> Array.filter (fun line -> line.Trim() <> "")

    let numbers =
        filteredLines
        |> Array.map (fun line ->
            line.Split([| ' '; '\t'; '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun str ->
                try
                    int str  // Try parsing each number
                with
                | :? System.FormatException -> 
                    printfn "Error parsing: %s" str
                    0  
            )
            |> Array.toList  // Convert inner array to list
        )
        |> Array.toList  // Convert outer array to list

    numbers

let getInputTransposed (day: string) (useExample: bool) : (int list list) = 
    let numbers = getInput day useExample

    let rec transpose lists =
        match lists with
        | []::_ -> []  // Stop when any list is empty
        | _ -> List.map List.head lists :: transpose (List.map List.tail lists)

    transpose numbers