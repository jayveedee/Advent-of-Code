module day02

let countSafeReports (input: int list list) (removeUnsafeLevel: bool) : string =
    let isStrictlyIncreasingWithStep lst =
        List.pairwise lst 
        |> List.forall (fun (a, b) -> a < b && b - a >= 1 && b - a <= 3)

    let isStrictlyDecreasingWithStep lst =
        List.pairwise lst 
        |> List.forall (fun (a, b) -> a > b && a - b >= 1 && a - b <= 3) 

    if removeUnsafeLevel then
        let generateSublists lst =
            [ for i in 0 .. List.length lst - 1 do
                yield List.take i lst @ List.skip (i + 1) lst ]

        let isValidAfterRemovingOne lst =
            generateSublists lst
            |> List.exists (fun sublist -> 
                isStrictlyIncreasingWithStep sublist || isStrictlyDecreasingWithStep sublist)

        let increasingCount, decreasingCount, validAfterRemovalCount =
            input
            |> List.fold (fun (inc, dec, rem) lst ->
                let isInc = isStrictlyIncreasingWithStep lst
                let isDec = isStrictlyDecreasingWithStep lst
                let isValidAfterRemoval = isValidAfterRemovingOne lst
                if isInc then 
                    (inc + 1, dec, rem) // Increment `inc`
                elif isDec then 
                    (inc, dec + 1, rem) // Increment `dec`
                elif isValidAfterRemoval then 
                    (inc, dec, rem + 1) // Increment `rem`
                else 
                    (inc, dec, rem) // No increment
            ) (0, 0, 0)

        string (increasingCount + decreasingCount + validAfterRemovalCount)
    else
        let increasingCount, decreasingCount =
            input
            |> List.fold (fun (inc, dec) lst ->
                if isStrictlyIncreasingWithStep lst then (inc + 1, dec)
                elif isStrictlyDecreasingWithStep lst then (inc, dec + 1)
                else (inc, dec)
            ) (0, 0)

        string (increasingCount + decreasingCount)



let run (part: int) (input: int list list) : string =
    match part with
    | 1 -> countSafeReports input false
    | 2 -> countSafeReports input true
    | _ -> failwith "part Argument should only be 1 or 2"