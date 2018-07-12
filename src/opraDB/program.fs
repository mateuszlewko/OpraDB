open OpraDB.AST
open OpraDB.Data
open OpraDB

open Hekate
open FSharpx

let printQueryResult str graph =
    // match run query str with
    // | Success (query, _, _)   ->
    let query = Parser.parseQuery str
    let ofId (ID s) = s
    let headers = query.basic.nodes |> List.map (NodeMatched.node >> ofId)
    let hsIdx   = headers |> List.indexed |> List.map swap |> Map.ofList
    let results =
        QueryExecution.matchedNodes graph query
        |> List.iter (printfn "----\n%A")
            // |> List.map (
            //     List.sortBy (fun (ID id, node) -> Map.tryFind id hsIdx)
            //     >> List.filter (fst >> ofId >> (flip Map.containsKey hsIdx))
            //     >> List.map (snd >> sprintf "%A"))

        // prettyTable results |> withHeaders headers |> sprintTable
        // |> printf "Query:\n\n%s\n\nResults:\n%s" str

        // let rowsCnt = List.length results
        // printfn "%d %s\n" rowsCnt (if rowsCnt <> 1 then "rows" else "row")
    ()
    // | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =

    let pathG : Graph =
        let me = Map.empty
        let edge = Map.ofList ["edge", String "link"]
        let edge2 = Map.ofList ["edge", String "link"
                                "a", String "ok"]

        // Graph.create // nodes
        //              [ 0, Map.ofList ["dest", String "end"]
        //                1, Map.ofList ["type", String "bus"]
        //                2, me
        //                3, me
        //                4, me
        //                5, me
        //                6, Map.ofList ["dest", String "end"]
        //                7, me
        //                8, Map.ofList ["type", String "bus"] 
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
                     [ 0, Map.ofList ["type", String "beg"
                                      "attr", Int 3]
                       4, Map.ofList ["type", String "beg"]
                       1, me
                       2, me
                       5, me
                       3, Map.ofList ["type", String "end"] 
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
                    LET crowded (x) =
                        MATCH NODES x SUCH THAT p : x -> y
                        WHERE route(p), y.attr > 100 HAVING SUM p BY <= 10 
                    IN 
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

    let pathQuery = "MATCH NODES s, t, x, y
                     SUCH THAT p: s->t, q : s -> y
                     WHERE 
                         (type(@p) = \"beg\").*
                       , .*(type(@p) = \"end\")
                       , (edge(@p, @'p) = \"link\")*.
                       , .*(a(@p, @'p) = \"ok\")
                          (a(@p, @'p) = \"ok\")
                          (a(@p, @'p) = \"ok\").*
                  
                       , (type(@q) = \"beg\").*
                       , .*(type(@q) = \"end\")
                       , (edge(@q, @'q) = \"link\")*.
                       , .*(a(@q, @'q) = \"ok\")
                          (a(@q, @'q) = \"ok\")
                          (a(@q, @'q) = \"ok\").*
                       HAVING 10 < 3 * (SUM p BY attr) + 4 * 2
                    "

    printQueryResult pathQuery pathG

    0
