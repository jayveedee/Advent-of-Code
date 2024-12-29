module day04

let convertToGrid (input: string list list) : char list list =
    input
    |> List.map (fun row -> Seq.toList (List.head row))

let findWordOccurrences (grid: char list list) (word: string) (xShaped: bool): string =
    let rows = List.length grid
    let cols = List.length (List.head grid)
    let directions = [ 
        (0, 1);    // Right
        (0, -1);   // Left
        (1, 0);    // Down
        (-1, 0);   // Up
        (1, 1);    // Diagonal-down-right
        (1, -1);   // Diagonal-down-left
        (-1, 1);   // Diagonal-up-right
        (-1, -1)   // Diagonal-up-left
    ]

    let isValid (r, c) = r >= 0 && c >= 0 && r < rows && c < cols

    let getChar (r, c) =
        if isValid (r, c) then Some (List.item c (List.item r grid)) else None

    let isXShape (r, c) =
        // Center, up-left, up-right, down-left, down-right
        match (getChar (r, c), getChar (r-1, c-1), getChar (r-1, c+1), getChar (r+1, c-1), getChar (r+1, c+1)) with
        | (Some 'A', Some 'M', Some 'M', Some 'S', Some 'S')
        | (Some 'A', Some 'M', Some 'S', Some 'M', Some 'S')
        | (Some 'A', Some 'S', Some 'M', Some 'S', Some 'M')
        | (Some 'A', Some 'S', Some 'S', Some 'M', Some 'M') -> true
        | _ -> false

    let rec checkWord (r, c) (dr, dc) idx =
        if idx = word.Length then true
        else
            match getChar (r + dr * idx, c + dc * idx) with
            | Some ch when ch = word.[idx] -> checkWord (r, c) (dr, dc) (idx + 1)
            | _ -> false

    let wordOccurrences =
        if xShaped then
            [ for r in 1 .. rows - 2 do // Ensure center 'A' is not on edges
                for c in 1 .. cols - 2 do
                    if isXShape (r, c) then
                        yield (r, c, (0, 0)) ] // We don't care about direction for X-shape
        else
            [ for r in 0 .. rows - 1 do
                for c in 0 .. cols - 1 do
                    for (dr, dc) in directions do
                        if checkWord (r, c) (dr, dc) 0 then
                            yield (r, c, (dr, dc)) ]

    string (List.length wordOccurrences)

let run (part: int) (input: string list list): string =
    let grid = convertToGrid input
    match part with
    | 1 -> findWordOccurrences grid "XMAS" false
    | 2 -> findWordOccurrences grid "XMAS" true
    | _ -> failwith "part Argument should only be 1 or 2"