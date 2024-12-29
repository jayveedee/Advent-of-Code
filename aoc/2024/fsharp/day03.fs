module day03

open System.Text.RegularExpressions

let findAllMulExpression (input: string): int =
    printfn "line: %s" input

    let regex = Regex(@"mul\((\d+),(\d+)\)")
    regex.Matches(input)
    |> Seq.cast<Match> 
    |> Seq.map (fun m ->
        let num1 = int (m.Groups.[1].Value)
        let num2 = int (m.Groups.[2].Value)
        printfn "num1: %i num2: %i" num1 num2
        num1 * num2
    )
    |> Seq.sum

let findAllowedMulExpression (input: string) (allowMultiply: bool): bool * int =
    let regex = Regex(@"(?<mul>mul\(\d+,\d+\))|(?<word>don't|do)", RegexOptions.Compiled)
    let matches = regex.Matches(input)
    
    let tokens = 
        matches
        |> Seq.cast<Match>
        |> Seq.map (fun m ->
            if m.Groups.["mul"].Success then m.Groups.["mul"].Value
            elif m.Groups.["word"].Success then m.Groups.["word"].Value
            else "") 
        |> Seq.filter (fun token -> token <> "") 
        |> Seq.toList

    let parseMul (token: string) =
        let inner = token.Substring(4, token.Length - 5)
        match inner.Split(',') with
        | [| num1; num2 |] -> Some (int num1, int num2)
        | _ -> None

    tokens
    |> List.fold (fun (allowMultiply, acc) token ->
        match token with
        | "don't" -> (false, acc) 
        | "do" -> (true, acc) 
        | _ ->
            match parseMul token with
            | Some (num1, num2) when allowMultiply -> (allowMultiply, acc + (num1 * num2)) 
            | _ -> (allowMultiply, acc) 
    ) (allowMultiply, 0) 

let multiplyCorruptedData(input: string list list) (partialUncorrupted: bool): string =
    let uncorruptedData = 
        if partialUncorrupted then
            let initialState = true 
            let data, _ = 
                input
                |> List.fold (fun (acc, allowMultiply) innerList ->
                    let innerSum, newState = 
                        innerList
                        |> List.fold (fun (innerAcc, currentAllowMultiply) line ->
                            let newAllowMultiply, lineSum = findAllowedMulExpression line currentAllowMultiply
                            (innerAcc + lineSum, newAllowMultiply)
                        ) (0, allowMultiply)
                    (acc + innerSum, newState)
                ) (0, initialState)
            data
        else
            input
            |> List.collect (fun innerList -> innerList |> List.map (fun line -> findAllMulExpression line))
            |> List.sum

    string uncorruptedData

let run (part: int) (input: string list list) : string =
    match part with
    | 1 -> multiplyCorruptedData input false
    | 2 -> multiplyCorruptedData input true
    | _ -> failwith "part Argument should only be 1 or 2"