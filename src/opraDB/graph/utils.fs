namespace OpraDB.Graph

open Hekate

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

    let allSimpleCycles graph = 
        ()