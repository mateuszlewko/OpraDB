﻿open OpraDB.Parser
open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.Data
open OpraDB

open FParsec
open MBrace.FsPickler
open Hekate
open PrettyTable
open FSharpx

let test p str =
    printf "%s => " str
    match run p str with
    | Success (result, _, _)   -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseAndRun = test query

let printQueryResult str graph =
    match run query str with
    | Success (query, _, _)   ->
        let ofId (ID s) = s
        let headers = query.nodes |> List.map ofId
        let hsIdx   = headers |> List.indexed |> List.map swap |> Map.ofList
        let results =
            QueryExecution.execute graph query
            // |> List.map (
            //     List.sortBy (fun (ID id, node) -> Map.tryFind id hsIdx)
            //     >> List.filter (fst >> ofId >> (flip Map.containsKey hsIdx))
            //     >> List.map (snd >> sprintf "%A"))

        // prettyTable results |> withHeaders headers |> sprintTable
        // |> printf "Query:\n\n%s\n\nResults:\n%s" str

        // let rowsCnt = List.length results
        // printfn "%d %s\n" rowsCnt (if rowsCnt <> 1 then "rows" else "row")
        ()
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    // test regularExpression ".(.+..*)+.."
    // test regularExpression ".*"
    // test regularExpression "(.*..)+."
    // test regularExpression "([attr(@1) > 10]*.)"
    // test regularExpression ".*..."
    // test regularExpression "(..)*.."
    // test regularExpression ".*.+."
    // test regularExpression ".*.+.+.."
    // test regularExpression "...(.(..)*.+.)...."

    // let q1 = "MATCH NODES (s t x y)
    //           SUCH THAT (s-[p1]->t x-[p2]->y)"

    // parseAndRun q1

    // let q2 = "MATCH NODES (s t)
    //           SUCH THAT (s-[p]->t)
    //           WHERE (..*<p>)"

    // parseAndRun q2

    // let q3 = "MATCH NODES (s t)
    //           SUCH THAT (s-[p]->t)
    //           WHERE ([attr(@1) >10] *. +..( .*)<p>)"

    // parseAndRun q3

    // let q4 = "MATCH NODES (s t)
    //           SUCH THAT (s-[p]->t s-[p2]->t)
    //           WHERE (.*[type (@1 @'1) = \"some type\"] *.<p>  .+..(.*)..<p p2>)"

    // parseAndRun q4

    // let q5 =  "MATCH NODES (s t)
    //            WHERE (.*[type (@1 @'1) = \"some type\"]*.<p>
    //                   .+(..(.*))..<p p2> )"
    // parseAndRun q5

    // let regexAst =
    //     ConcatExp
    //         (AnyExp, // .
    //          ConcatExp
    //             (StarExp AnyExp, // .*
    //              EpsilonExp))

    // State.ofRegExp regexAst |> printfn "%A"

    // let regexAst =
    //     ConcatExp
    //         (AnyExp, // .
    //          ConcatExp
    //             (StarExp AnyExp, // .*
    //              ConcatExp
    //                 (AnyExp,     // .
    //                  AnyExp)))   // .

    // let nfa = State.ofRegExp regexAst
    // nfa |> printfn "%A"

    // let ser = FsPickler.CreateXmlSerializer (indent = true)
    // printfn "Serialized: %s\n" (ser.PickleToString nfa)

    
    let pathG : Graph =
        let me = Map.empty
        let edge = Map.ofList ["edge", StringVal "link"]
        let edge2 = Map.ofList ["edge", StringVal "link"
                                "a", StringVal "ok"]

        // Graph.create // nodes
        //              [ 0, Map.ofList ["dest", StringVal "end"]
        //                1, Map.ofList ["type", StringVal "bus"]
        //                2, me
        //                3, me
        //                4, me
        //                5, me
        //                6, Map.ofList ["dest", StringVal "end"]
        //                7, me
        //                8, Map.ofList ["type", StringVal "bus"] 
        //              ]                  
        //              // edges
        //              [ 0, 8, edge
        //                1, 2, edge 
        //                2, 3, edge 
        //                3, 4, edge 
        //                4, 5, edge
        //                3, 6, edge
        //                8, 2, edge
        //                6, 2, edge 
        //                6, 0, edge 
        //              ]

        Graph.create // nodes
                     [ 0, Map.ofList ["type", StringVal "beg"]
                       4, Map.ofList ["type", StringVal "beg"]
                       1, me
                       2, me
                       5, me
                       3, Map.ofList ["type", StringVal "end"] 
                     ]                  
                     // edges
                     [ //0, 8, edge
                       0, 1, edge 
                       4, 5, edge 
                       5, 0, edge 
                       1, 2, edge2 
                       2, 1, edge2
                       2, 3, edge
                     ]

    // let pathQuery = "MATCH NODES (s t)                        \
    //                \nSUCH THAT (s-[p]->t)                     \
    //                \nWHERE ([type(@1) = \"bus\"].*<p>         \
    //                \n       .*[dest(@1) = \"end\"]<p>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<p> )"

    // printQueryResult pathQuery pathG

    // let pathQuery = "MATCH NODES (s t)                        \
    //                \nSUCH THAT (s-[p]->t)           \
    //                \nWHERE ([type(@1) = \"beg\"].*<p>         \
    //                \n       .*[dest(@1) = \"end\"]<p>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<p>    \
    //                \n       .*[a(@1 @'1) = 1][a(@1 @'1) = 1]*.<p>    \
    //                \n                                         \
    //                \n       [type(@1) = \"beg\"].*<q>         \
    //                \n       .*[dest(@1) = \"end\"]<q>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<q> )"

    let pathQuery = "
                    LET connected x y = (@x.type = 0) * @x.time IN
                    LET route p = ((@p, @'p).edge = true)*. IN
                    
                    MATCH NODES s, t, y PATHS p, q
                    SUCH THAT p: s->t, q: x->y, r: a..b
                    WHERE 
                        s.type = \"beg\"
                        AND t.type = \"end\",
                        AND type(@p) = \"place\"*
                        AND @p.type = \"place\"
                        AND ((@p, @' q).edge = \"link\")*.
                        AND .*((@p, @q).a = \"ok\"){3}.*
                        AND (connected(@p, @q))*

                          s.type = \"beg\"
                        , t.type = \"end\"
                        , ((@p, @'q).edge = \"link\")*.
                        , .*((@p, @q).a = \"ok\"){3}.*
                        , (connected(@p, @q))*
                    HAVING 
                        SUM p BY atrr <= 10
                        AND SUM p BY attr = MAX route(s, t, p) BY attr
                        AND MAX p BY SUM OF attr 
                    ;

                    "

    let pathQuery = "MATCH NODES (s t)
                    SUCH THAT (s-[p]->t x-[q]->y)
                    WHERE (
                        [type(@1) = \"beg\"].*<p>
                        .*[type(@1) = \"end\"]<p>
                        [edge(@1 @'1) = \"link\"]*.<p>
                        .*[a(@1 @'1) = \"ok\"][a(@1 @'1) = \"ok\"][a(@1 @'1) = \"ok\"].*<p>
                        
                        [type(@1) = \"beg\"].*<q>
                        .*[type(@1) = \"end\"]<q>
                        [edge(@1 @'1) = \"link\"]*.<q>
                        .*[a(@1 @'1) = \"ok\"][a(@1 @'1) = \"ok\"][a(@1 @'1) = \"ok\"].*<q>
                    )
                    "

    // let pathQuery = "MATCH NODES (s t)                        \
    //                \nSUCH THAT (p: s->t, s1: a..b )           \
    //                \nWHERE ([type(@1) = \"bus\"].*<p>         \
    //                \n       .*[dest(@1) = \"end\"]<p>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<p>    \
    //                \n                                         \
    //                \n       [type(@1) = \"bus\"].*<q>         \
    //                \n       .*[dest(@1) = \"end\"]<q>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<q> )"

    // let pathQuery = "MATCH NODES (s t)                        \
    //                \nSUCH THAT (s->t as p, a..b as s1)           \
    //                \nWHERE ([type(@p) = \"bus\"].*          \
    //                \n       .*[dest(@p) = \"end\"]         \
    //                \n       [edge(@p @'p) = \"link\"]*.<p>    \
    //                \n                                         \
    //                \n       [type(@1) = \"bus\"].*<q>         \
    //                \n       .*[dest(@1) = \"end\"]<q>         \
    //                \n       [edge(@1 @'1) = \"link\"]*.<q> )"

    printQueryResult pathQuery pathG

    // let pathQuery = "MATCH NODES (u v)                        \
    //                \nSUCH THAT (u-[p]->v)                     \
    //                \nWHERE ([type(@1) = \"bus\"].*<p>         \
    //                \n       .*([dest(@1) = \"end\"] + .)<p>   \
    //                \n       [edge(@1 @'1) = \"link\"]*.<p> )"

    // printQueryResult pathQuery pathG

    0
