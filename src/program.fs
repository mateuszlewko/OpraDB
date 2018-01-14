open OpraDB.Parser
open OpraDB.LangTypes
open OpraDB.RegexNFA
open OpraDB.Data
open OpraDB

open FParsec
open MBrace.FsPickler
open Hekate

let test p str =
    printf "%s => " str
    match run p str with
    | Success (result, _, _)   -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseAndRun = test query

let printQueryResult str graph =
    match run query str with
    | Success (query, _, _)   ->
        printfn " -- RESULTS -- "

        for result in QueryExecution.execute graph query do
            printfn " { "
            for (ID id, v) in result do
                printfn "\t%s => %d" id v
            printfn " } "

        printfn ""
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test regularExpression ".(.+..*)+.."
    test regularExpression ".*"
    test regularExpression "(.*..)+."
    test regularExpression "([attr(@1) > 10]*.)"
    test regularExpression ".*..."
    test regularExpression "(..)*.."
    test regularExpression ".*.+."
    test regularExpression ".*.+.+.."
    test regularExpression "...(.(..)*.+.)...."

    let q1 = "MATCH NODES (s t x y)
              SUCH THAT (s-[p1]->t x-[p2]->y)"

    parseAndRun q1

    let q2 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t)
              WHERE (..*<p>)"

    parseAndRun q2

    let q3 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t)
              WHERE ([attr(@1) >10] *. +..( .*)<p>)"

    parseAndRun q3

    let q4 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t s-[p2]->t)
              WHERE (.*[type (@1 @'1) = \"some type\"] *.<p>  .+..(.*)..<p p2>)"

    parseAndRun q4

    let q5 =  "MATCH NODES (s t)
               WHERE (.*[type (@1 @'1) = \"some type\"]*.<p>
                      .+(..(.*))..<p p2> )"
    parseAndRun q5

    let regexAst =
        ConcatExp
            (AnyExp, // .
             ConcatExp
                (StarExp AnyExp, // .*
                 EpsilonExp))

    State.ofRegExp regexAst |> printfn "%A"

    let regexAst =
        ConcatExp
            (AnyExp, // .
             ConcatExp
                (StarExp AnyExp, // .*
                 ConcatExp
                    (AnyExp,     // .
                     AnyExp)))   // .

    let nfa = State.ofRegExp regexAst
    nfa |> printfn "%A"

    let ser = FsPickler.CreateXmlSerializer (indent = true)
    printfn "Serialized: %s\n" (ser.PickleToString nfa)

    let pathG : Graph =
        let me = Map.empty
        let edge = Map.ofList ["edge", StringVal "link"]

        Graph.create [1, Map.ofList ["type", StringVal "bus"]; 2, me; 3, me;
                      4, me; 5, me; 10, Map.ofList ["dest", StringVal "end"];
                      11, me; 12, Map.ofList ["type", StringVal "bus"];]
                     [1, 2, edge; 2, 3, edge; 3, 4, edge; 4, 5, edge;
                      3, 10, edge; 12, 2, edge]

    let pathQuery = "MATCH NODES (s t)
                     SUCH THAT (s-[p]->t)
                     WHERE ([type(@1) = \"bus\"].*<p>
                            .*[dest(@1) = \"end\"]<p>
                            [edge(@1 @'1) = \"link\"]*.<p> )"

    printQueryResult pathQuery pathG

    let pathQuery = "MATCH NODES (u v)
                     SUCH THAT (u-[p]->v)
                     WHERE ([type(@1) = \"bus\"].*<p>
                            .*([dest(@1) = \"end\"] + .)<p>
                            [edge(@1 @'1) = \"link\"]*.<p> )"

    printQueryResult pathQuery pathG

    0
