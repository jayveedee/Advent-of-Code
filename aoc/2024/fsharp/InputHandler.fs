module InputHandler

open System.IO

let getInput (day: string) : (int list list) = 
    let dir = Directory.GetCurrentDirectory()
    let path = Path.Combine(dir, "..", "inputs", $"day{day}.txt")
    let fullPath = Path.GetFullPath(path)

    printfn "Getting input from: %s" fullPath
    let lines = File.ReadAllLines(fullPath)

    let filteredLines = lines |> Array.filter (fun line -> line.Trim() <> "")

    let numbers =
        filteredLines
        |> Array.map (fun line ->
            line.Split("   ")
            |> Array.map (fun str ->
                try
                    int str  // Try parsing each number
                with
                | :? System.FormatException -> 
                    printfn "Error parsing: %s" str
                    0  // Handle parsing errors (e.g., set to 0 or skip)
            )
            |> Array.toList
        )

    let rec transpose lists =
        match lists with
        | []::_ -> []
        | _ -> List.map List.head lists :: transpose (List.map List.tail lists)

    transpose (Array.toList numbers) 