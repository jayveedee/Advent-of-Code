module day05

let checkGroupOrdering (pairs: (int * int) list) (group: int list): bool =
    let groupSet = Set.ofList group
    let validPairs = 
        pairs 
        |> List.filter (fun (first, second) -> Set.contains first groupSet && Set.contains second groupSet)
    
    let indexMap = group |> List.mapi (fun i num -> num, i) |> Map.ofList
    //printfn "Group: %A" group
    //printfn "Valid Pairs: %A" validPairs
    //printfn "Index Map: %A" indexMap
    
    validPairs
    |> List.forall (fun (first, second) ->
        match Map.tryFind first indexMap, Map.tryFind second indexMap with
        | Some i1, Some i2 -> 
            printfn "Checking pair (%i, %i): i1 = %i, i2 = %i" first second i1 i2
            i1 < i2
        | _ -> 
            printfn "Unexpected missing pair (%i, %i) in index map" first second
            false)

let findMiddle (group: int list) =
    let len = List.length group
    if len % 2 = 1 then
        group.[len / 2] 
    else
        group.[len / 2 - 1] 

let reorderGroup (pairs: (int * int) list) (group: int list) =
    let groupSet = Set.ofList group

    let checkPairOrdering (first, second) =
        match List.tryFindIndex ((=) first) group, List.tryFindIndex ((=) second) group with
        | Some i1, Some i2 -> i1 < i2
        | _ -> false

    let validPairs = 
        pairs 
        |> List.filter (fun (first, second) -> Set.contains first groupSet && Set.contains second groupSet)

    let invalidPairs = validPairs |> List.filter (fun pair -> not (checkPairOrdering pair))

    // If there are no invalid pairs, return None
    if List.isEmpty invalidPairs then
        None
    else
        let adjacencyMap =
            validPairs
            |> List.fold (fun acc (first, second) ->
                let updated = Map.add first (Set.add second (Map.tryFind first acc |> Option.defaultValue Set.empty)) acc
                Map.add second (Map.tryFind second acc |> Option.defaultValue Set.empty) updated) Map.empty

        let rec topologicalSort visited stack = function
            | [] -> visited, stack
            | x :: xs when Set.contains x visited -> topologicalSort visited stack xs
            | x :: xs ->
                let neighbors = Map.tryFind x adjacencyMap |> Option.defaultValue Set.empty |> Set.toList
                let visited', stack' = topologicalSort visited stack neighbors
                topologicalSort (Set.add x visited') (x :: stack') xs

        // Perform topological sort to reorder the group
        let _, reorderedGroup =
            topologicalSort Set.empty [] group

        let reorderedGroupFiltered =
            reorderedGroup
            |> List.rev // Topological sort adds elements in reverse order
            |> List.filter (fun x -> Set.contains x groupSet) // Ensure only original group elements are included

        printfn "Reordered group: %A" reorderedGroupFiltered
        Some reorderedGroupFiltered

let correctlyReorderedUpdates (rules: (int * int) list) (pages: int list list): string =
    let sumMiddleNumbers pairs groups =
        groups
        |> List.choose (reorderGroup pairs)
        |> List.map findMiddle 
        |> List.sum 

    string (sumMiddleNumbers rules pages)

let correctlyOrderedUpdates (rules: (int * int) list) (pages: int list list): string =
    let validGroups =
        pages
        |> List.filter (checkGroupOrdering rules) 

    let middleElements =
        validGroups
        |> List.map findMiddle 

    let sumOfMiddleElements = List.sum middleElements

    //printfn "Valid Groups: %A" validGroups
    printfn "Middle Elements: %A" middleElements

    string sumOfMiddleElements

let splitInput (input: string list): (int * int) list * int list list =
    let pairs, groups =
        input
        |> List.filter (fun line -> line <> "") 
        |> List.partition (fun line -> line.Contains("|"))

    let splitPairs = pairs |> List.map (fun pair -> pair.Split('|') |> Array.map int |> (fun arr -> arr.[0], arr.[1]))
    let splitGroups = groups |> List.map (fun group -> group.Split(',') |> Array.map int |> Array.toList)

    splitPairs, splitGroups

let run (part: int) (input: string list) : string =
    let rules, pages = splitInput input

    printfn "Rules:\n%A" rules
    printfn "\nPages:\n%A" pages

    match part with
    | 1 -> correctlyOrderedUpdates rules pages
    | 2 -> correctlyReorderedUpdates rules pages
    | _ -> failwith "part Argument should only be 1 or 2"