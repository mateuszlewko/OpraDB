namespace OpraDB.Graph

open Hekate
open FSharpx.Option
open System.Collections.Generic
open FSharpx
open System.Linq

module Utils = 
    
    let restoreGraph preds sink = 
        let rec restore node visited graph = 
            if Set.contains node visited
            then graph, visited
            else let visited = Set.add node visited 
                 match Map.tryFind node preds with 
                 | None     -> graph, visited
                 | Some pre -> 
                     Set.fold (fun (graph, vis) p -> 
                             let tryAdd n g =
                                 if Graph.Nodes.contains n g 
                                 then g else Graph.Nodes.add (n, ()) g
 
                             tryAdd p graph |> tryAdd node 
                             |> Graph.Edges.add (p, node, ()) 
                             |> restore p vis
                         ) (graph, visited) pre
        
        restore sink Set.empty Graph.empty

    let dfs start graph postVisit = 
        let visited = HashSet () 

        let rec visit u = 
            if not (visited.Contains u) 
            then visited.Add u |> ignore
                 for v, _ in Graph.Nodes.successors u graph |> getOrElse []
                    do visit v

                 postVisit u
                 
        visit start
        visited.ToFSharpList ()

    let postOrderTimes graph = 
        let postOrder = Dictionary ()
        let cnt       = ref 0
        let nodes     = HashSet (Graph.Nodes.toList graph |> List.map fst)

        let rec visitAll graph =
            if not (Graph.isEmpty graph)
            then 
                let start = nodes.First ()
                let vis   = dfs start graph (fun u -> postOrder.Add (u, !cnt)
                                                      cnt := !cnt + 1
                                                      nodes.Remove u |> ignore) 
                let graph = Graph.Nodes.removeMany vis graph
                visitAll graph

        visitAll graph
        postOrder.ToFSharpList () |> List.map (fun kv -> kv.Key, kv.Value)

    let stronglyConnectedComponents graph =
        let orderedNodes = postOrderTimes graph 
                           |> List.sortByDescending snd
                           |> List.map fst

        let rec nextScc graph vis sccs =
            function 
            | [] | _ when Graph.isEmpty graph        -> sccs
            | node::nodes when Set.contains node vis -> 
                nextScc graph vis sccs nodes
            | node::nodes -> let scc   = dfs node graph ignore 
                             let graph = Graph.Nodes.removeMany scc graph
                             let vis   = Set.union (Set.ofList scc) vis
                             nextScc graph vis (scc::sccs) nodes 
        
        nextScc (Graph.rev graph) Set.empty [] orderedNodes

    let allSimpleCycles graph = 
        ()