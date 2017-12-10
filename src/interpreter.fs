namespace OpraDB 

open OpraDB.LangTypes

module Interpreter = 
    let interpret (query : Query) = 
        let nodes = Set.ofList query.nodes

        for p in query.pathConstraints do 
            if Set.contains p.source nodes 
            then 
                if Set.contains p.target nodes 
                then printfn "Looking for path %A from %A to %A" 
                        p.path p.source p.target
                else printfn "ERROR: target node %A not in matched nodes" p.target
            else printfn"ERROR: source node %A not in matched nodes" p.source
            