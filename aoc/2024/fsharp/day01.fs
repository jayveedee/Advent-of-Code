module day01

let findDifferences (listOfLists: int list list) : string =
    match listOfLists with
    | [list1; list2] when List.length list1 = List.length list2 ->
        let sortedList1 = List.sort list1
        let sortedList2 = List.sort list2

        let differences = 
            List.map2 (fun x1 x2 ->
                let diff = abs (x2 - x1)
                printfn "x1: %i, x2: %i, difference: %i" x1 x2 diff
                diff 
            ) sortedList1 sortedList2

        let sumOfDifferences = List.sum differences

        string sumOfDifferences
    | _ -> 
        failwith "The input should be a list of exactly two lists of the same length."

let findSimilarityScore (listOfLists: int list list) : string =
    match listOfLists with
    | [list1; list2] when List.length list1 = List.length list2 -> 
        let countLeft = List.countBy id list1
        let countRight = List.countBy id list2

        countLeft |> List.iter (fun x -> printfn "countLeft: %d %d" (fst x) (snd x))
        countRight |> List.iter (fun x -> printfn "countRight: %d %d" (fst x) (snd x))

        let result = 
            countLeft
            |> List.fold (fun acc (value, leftCount) ->
                match List.tryFind (fun (v, _) -> v = value) countRight with
                | Some (_, rightCount) -> 
                    let similarityScore = acc + (value * leftCount * rightCount)
                    printfn "value: %i leftCount: %i, rightCount: %i, similarity score: %i" value leftCount rightCount similarityScore
                    similarityScore
                | None -> 
                    let similarityScore = acc + (value * leftCount * 0)  // No match in countRight
                    printfn "value: %i leftCount: %i, rightCount: 0, similarity score: %i" value leftCount similarityScore
                    similarityScore
            ) 0

        string result
    | _ -> 
        failwith "The input should be a list of exactly two lists of the same length."

let part01 (input: int list list) : string =
    findDifferences input

let part02 (input: int list list) : string =
    findSimilarityScore input

let run (part: int) (input: int list list) : string =
    match part with
    | 1 -> part01 input
    | 2 -> part02 input
    | _ -> failwith "part Argument should only be 1 or 2"