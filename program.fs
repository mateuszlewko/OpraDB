// open System
open OpraDB.Interpreter
open OpraDB.Parser
open FParsec 

let test p str =
    match run p str with
    | Success (result, _, _)   -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseAndRun str = 
    match run parseQuery str with 
    | Success (result, _, _) -> interpret result 
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test (manyWith id "NODES") "NODES (Ba Ab aa)"
    test (manyWith id "NODES") "NODES (s t )"
    test (manyWith pathConstraint "SUCH THAT") "SUCH THAT (s -[pii]-> t x-[p]->y)"

    let q1 = "MATCH NODES (s t x y)
              SUCH THAT (s -[pii]-> t x-[p]->y )"

    test parseQuery q1 
    parseAndRun q1 

    let q2 = "MATCH NODES (s t x y)
              SUCH THAT (s -[p1]-> t x-[p2]->y u-[ p3 ]->v)"

    parseAndRun q2
    0 // return an integer exit code    
