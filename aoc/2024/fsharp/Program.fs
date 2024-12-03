module Program

open InputHandler
open day01
open day02
open day03

let parseDayAndPart (day: string) (part: string) =
    match System.Int32.TryParse(day), System.Int32.TryParse(part) with
    | (true, dayInt), (true, partInt) -> Some(dayInt, partInt)
    | _ -> None

// dotnet run -- <day> <part> <use example>
[<EntryPoint>]
let main argv =
    let useExample = Array.length argv = 3
    match argv with
    | [| day; part |] | [| day; part; _ |] ->  
        match parseDayAndPart day part with
        | Some(dayInt, partInt) ->  
            let result = 
                match dayInt with
                | 1 -> day01.run partInt (InputHandler.getPuzzleInputT day useExample)
                | 2 -> day02.run partInt (InputHandler.getPuzzleInput day useExample)
                | 3 -> day03.run partInt (InputHandler.getPuzzleInput day useExample)
                | _ -> failwith "Error: Day not implemented."

            printfn "Result for day%d part%d: %s" dayInt partInt result
            0
        | None -> failwith "Error: Day or part is not a valid integer."
    | _ -> failwith "Usage: MyApp <day> <part>"