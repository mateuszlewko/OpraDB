// Learn more about F# at http://fsharp.org

open System
open OpraDB.Lang
open FParsec 

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test (manyIDWith "NODES") "NODES Ba Ab aa"
    0 // return an integer exit code
