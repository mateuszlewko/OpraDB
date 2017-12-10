open OpraDB.Interpreter
open OpraDB.Parser
open FParsec 

let test p str =
    printf "%s => " str
    match run p str with
    | Success (result, _, _)   -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseAndRun str = 
    match run parseQuery str with 
    | Success (result, _, _)   -> interpret result 
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    // test (manyWith id "NODES") "NODES (Ba Ab aa)"
    // test (manyWith id "NODES") "NODES (s t )"
    // test (manyWith pathConstraint "SUCH THAT") "SUCH THAT (s -[pii]-> t x-[p]->y)"
    test RegParser2.expr ".^(.+.^.*)+.^."

    test RegexParser.regExpParser ".*"
    test RegexParser.regExpParser "(.*..)+."
    test RegexParser.regExpParser "([attr(@1) > 10]*.)"
    test RegexParser.regExpParser ".*..."
    test RegexParser.regExpParser "(..)*.."
    test RegexParser.regExpParser ".*.+."
    test RegexParser.regExpParser ".*.+.+.."
    test RegexParser.regExpParser "...(.(..)*.+.)...."


    run RegexParser.regExpParser ".*..." 
    |> function
       |  Success (result, _, _) -> RegexParser.parseReg [] result |> printfn "%A"

    run RegexParser.regExpParser "([attr(@1) > 10]*.)" 
    |> function
       |  Success (result, _, _) -> RegexParser.parseReg [] result |> printfn "%A"

    // test nodeConstraint "[attr(@1) = 10]"
    // test nodeConstraint "[name(@1) = \"some value\"]"


    // let q1 = "MATCH NODES (s t x y)
    //           SUCH THAT (s -[pii]-> t x-[p]->y )"

    // test parseQuery q1 
    // parseAndRun q1 

    // let q2 = "MATCH NODES (s t x y)
    //           SUCH THAT (s-[p1]->t x-[p2]->y u-[ p3 ]->v)
    //           WHERE "

    // parseAndRun q2

    // let q3 = "MATCH NODES (s t)
    //           SUCH THAT (s-[p]->t)
    //           WHERE ( ) "

    // parseAndRun q2

    0
