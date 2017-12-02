open System
open OpraDB.Lang
open FParsec 

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test (manyWith id "NODES") "NODES Ba Ab aa"
    test (manyWith id "NODES") "NODES s t "
    test (manyWith pathConstraint "SUCH THAT") "SUCH THAT s -[pii]-> t x-[p]->y "

    0 // return an integer exit code
