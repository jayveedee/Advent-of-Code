module day01

let findDifferences (listOfLists: string list list) : string =
    match listOfLists with
    | [list1; list2] when List.length list1 = List.length list2 ->
        let sortedList1 = List.sort list1
        let sortedList2 = List.sort list2

        let differences = 
            List.map2 (fun x1 x2 ->
                let diff = abs (int x2 - int x1)
                printfn "x1: %s, x2: %s, difference: %i" x1 x2 diff
                diff 
            ) sortedList1 sortedList2

        let sumOfDifferences = List.sum differences

        string sumOfDifferences
    | _ -> 
        failwith "The input should be a list of exactly two lists of the same length."

let findSimilarityScore (listOfLists: string list list) : string =
    match listOfLists with
    | [list1; list2] when List.length list1 = List.length list2 -> 
        let countLeft = List.countBy id list1
        let countRight = List.countBy id list2

        countLeft |> List.iter (fun x -> printfn "countLeft: %s %i" (fst x) (snd x))
        countRight |> List.iter (fun x -> printfn "countRight: %s %i" (fst x) (snd x))

        let result = 
            countLeft
            |> List.fold (fun acc (value, leftCount) ->
                match List.tryFind (fun (v, _) -> v = value) countRight with
                | Some (_, rightCount) -> 
                    let similarityScore = acc + (int value * leftCount * rightCount)
                    printfn "value: %s leftCount: %i, rightCount: %i, similarity score: %i" value leftCount rightCount (leftCount * rightCount)
                    similarityScore
                | None -> 
                    let similarityScore = acc + (int value * leftCount * 0)  
                    printfn "value: %s leftCount: %i, rightCount: 0, similarity score: %i" value leftCount (leftCount * 0)
                    similarityScore
            ) 0

        string result
    | _ -> 
        failwith "The input should be a list of exactly two lists of the same length."

let run (part: int) (input: string list list) : string =
    match part with
    | 1 -> findDifferences input
    | 2 -> findSimilarityScore input
    | _ -> failwith "part Argument should only be 1 or 2"