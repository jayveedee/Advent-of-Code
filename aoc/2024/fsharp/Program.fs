module Program

open InputHandler
open day01

let run (day: int) (part: int) (input: int list list) : string =
    match day with
    | 1 -> day01.run part input
    | _ -> failwith (sprintf "Error: day%i not implemented." day)

let parseDayAndPart (day: string) (part: string) =
    match System.Int32.TryParse(day), System.Int32.TryParse(part) with
    | (true, dayInt), (true, partInt) -> Some(dayInt, partInt)
    | _ -> None

// dotnet run -- <day> <part> <use example>
[<EntryPoint>]
let main argv =
    match argv with
    | [| day; part |] | [| day; part; _ |] ->  
        match parseDayAndPart day part with
        | Some(dayInt, partInt) ->  
            let input = if Array.length argv = 3 then InputHandler.getInput "0" else InputHandler.getInput day
            printfn "Result for day%d part%d%s: %s" dayInt partInt (if Array.length argv = 3 then " example" else "") (run dayInt partInt input)
            0
        | None -> failwith "Error: Day or part is not a valid integer."
    | _ -> failwith "Usage: MyApp <day> <part>"