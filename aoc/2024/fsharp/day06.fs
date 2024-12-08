module day06

let avoidParadoxes (grid: string list): string =
        // Define possible directions: up, right, down, left
    let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]

    // Get the grid dimensions
    let rows = grid.Length
    let cols = grid.[0].Length

    // Find the start position and direction
    let findStart () =
        let mutable startPos = None
        // Ensure the grid is non-empty
        if grid.Length > 0 then
            for r in 0 .. grid.Length - 1 do
                let row = grid.[r]
                // Ensure the row is non-empty
                for c in 0 .. row.Length - 1 do
                    if grid.[r].[c] = '^' then
                        startPos <- Some (r, c, 0) // Start facing up
        match startPos with
        | Some pos -> pos
        | None -> failwith "Starting position '^' not found in the grid."

    // Check if a position is within bounds
    let isInBounds (r, c) = r >= 0 && r < rows && c >= 0 && c < cols

    // Navigate and track distinct elements
    let rec navigate (r, c, dir, visited) =
        let dr, dc = directions.[dir]
        let nr, nc = r + dr, c + dc

        if not (isInBounds (nr, nc)) then
            // Reached the edge
            visited
        elif grid.[nr].[nc] = '#' then
            // Hit an obstacle, change direction clockwise
            let newDir = (dir + 1) % 4
            navigate (r, c, newDir, visited)
        else
            // Move forward, add current cell to visited set
            let newVisited = Set.add (nr, nc) visited
            navigate (nr, nc, dir, newVisited)

    // Get the starting position and direction
    let startRow, startCol, startDir = findStart ()
    let initialVisited = Set.singleton (startRow, startCol)
    let distinctVisited = navigate (startRow, startCol, startDir, initialVisited)
    
    string (Set.count distinctVisited)

let run (part: int) (input: string list) : string =
    match part with
    | 1 -> avoidParadoxes input
    | 2 -> ""
    | _ -> failwith "part Argument should only be 1 or 2"