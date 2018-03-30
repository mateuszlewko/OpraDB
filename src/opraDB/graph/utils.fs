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
            | []                                     -> sccs
            | _ when Graph.isEmpty graph             -> sccs
            | node::nodes when Set.contains node vis -> 
                nextScc graph vis sccs nodes
            | node::nodes                            -> 
                let scc   = dfs node graph ignore 
                let graph = Graph.Nodes.removeMany scc graph
                let vis   = Set.union (Set.ofList scc) vis
                nextScc graph vis (scc::sccs) nodes 
        
        nextScc (Graph.rev graph) Set.empty [] orderedNodes

    let allSimpleCycles graph = 
        let blocked = HashSet ()
        let unblock = Dictionary () : Dictionary<_, List<_>>
        let cycles  = List ()
        let sccs    = stronglyConnectedComponents graph

        let dictGetOr thunk (dict : #Dictionary<_, _>) key =
            if dict.ContainsKey key 
            then dict.[key]
            else thunk ()

        let rec unblockNode u = 
            if blocked.Remove u
            then Seq.iter unblockNode (dictGetOr List unblock u)
                 unblock.Remove u |> ignore

        let addUnblockers u =
            List.iter (fun x -> (dictGetOr List unblock x).Add u)
        
        let rec cyclesOfScc graph startNode = 
            let rec visit stack u = 
                if not (blocked.Contains u) 
                then 
                    blocked.Add u |> ignore
                    let stack = u::stack
                    let ns    = Graph.Nodes.successors u graph 
                                |> getOrElse [] |> List.map fst

                    let foundCycle  = List.exists ((=) startNode) ns
                    let block, free = List.partition blocked.Contains ns 

                    if List.isEmpty free 
                    then addUnblockers u block
                     
                    let isCycle = free |> List.map (fun v ->
                                           if blocked.Contains v 
                                           then cycles.Add (v::stack)
                                           visit stack v)

                    if foundCycle then cycles.Add stack

                    if foundCycle || List.exists id isCycle 
                    then unblockNode u; true 
                    else false
                else false
            
            visit [] startNode

        let rec getAllCycles graph = 
            function
            | []                   -> () 
            | []::sccs             -> getAllCycles graph sccs |> ignore
            | (startNode::_)::sccs ->
                cyclesOfScc graph startNode |> ignore
                getAllCycles (Graph.Nodes.remove startNode graph) sccs
                 
        getAllCycles graph sccs
        cycles.ToFSharpList ()