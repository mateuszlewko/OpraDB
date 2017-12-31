namespace OpraDB

open OpraDB.LangTypes
open FSharpx
open Hekate

module RegexNFA = 

    type State = 
        | Matched // final node
        | Constraint of NodeConstraint // need to check NodeConstraint
        | Any // any node will match
        | Empty // go to next state without consuming any node

    module State = 

        let ofRegExp =
            let addNode lastID addLast neighbours state graph = 
                let currID = lastID + 1
                let edges = List.map (fun n -> currID, n, ()) neighbours

                Graph.Nodes.add (currID, state) graph 
                |> Graph.Edges.addMany 
                    (if addLast then (lastID, currID, ())::edges else edges)
                , currID

            let rec build graph lastID addLast neighbours =
                function 
                | EpsilonExp -> 
                    addNode lastID addLast neighbours Matched graph, []
                | AnyExp         -> 
                    addNode lastID addLast neighbours Any graph, []
                | NodeExp constr -> 
                    addNode lastID addLast neighbours (Constraint constr) graph
                    , []
                | ConcatExp (e1, e2) -> 
                    let (graph, lastID), neighbours = 
                        build graph lastID neighbours e1
                    build graph lastID neighbours e2
                | StarExp e          -> 
                    let graph, lastID = addNode lastID addLast neighbours Empty graph
                    build graph lastID [lastID] e
                | UnionExp (e1, e2)  ->
                    let currID = lastID + 1 
                    let (graph, leftID), leftNeighbours = 
                        build graph currID neighbours e1
                    let graph, rightID, neighbours =
                        build graph leftID neighbours e2
      
            build Graph.empty -1 false [] >> fst >> fst