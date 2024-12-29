module day07

let parseInput (input: string list) =
    let filteredInput = input |> List.filter (fun line -> line.Trim() <> "")

    filteredInput
    |> List.map (fun line ->
        let parts = line.Split(':') 
        let result = int64 (parts.[0].Trim()) 
        let numbers = 
            parts.[1].Trim().Split(' ') 
            |> Array.map int64
            |> Array.toList 
        (result, numbers)) 

let determineEquationIsPossible(input: (int64 * int64 list) list) (concatinate: bool): string =
    let rec findTarget target nums =
        // If only one number is left, check if it matches the target
        if List.length nums = 1 then
            List.head nums = target
        else
            // Try every possible pair of numbers
            let rec tryOperators nums =
                match nums with
                | x :: y :: rest -> 
                    // Try addition first
                    if findTarget target ((x + y) :: rest) then true
                    // Try multiplication next
                    else if findTarget target ((x * y) :: rest) then true
                    // Try concatination last
                    else if concatinate then
                        let concatenated = int64 (string x + string y)  
                        if findTarget target (concatenated :: rest) then true
                        else tryOperators (y :: rest)
                    // Try next operator
                    else tryOperators (y :: rest)
                | _ -> false 
            tryOperators nums

    let results = 
        input
        |> List.map (fun (result, numbers) ->
            let isPossible = findTarget result numbers
            (result, isPossible))

    string (results |> List.fold (fun (acc: int64) (result, isPossible) -> if isPossible then acc + result else acc) 0)

let run (part: int) (input: string list) : string =
    let parsedInput = parseInput input
    match part with
    | 1 -> determineEquationIsPossible parsedInput false
    | 2 -> determineEquationIsPossible parsedInput true
    | _ -> failwith "part Argument should only be 1 or 2"
