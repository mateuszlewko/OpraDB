namespace OpraDB

open OpraDB.RegularConstraints
open OpraDB.QueryData
open OpraDB.AST
open FSharpx
open FSharpx.Collections

module QueryExecution =

    let matchedNodes graph (query : Query) =
        let pathConstraints =
            List.map (fun c -> c.path, c) query.basic.pathConstraints 
            |> Map.ofList
        
        let getStartEndNodes (mEdge : MatchedEdge) =
            Map.tryFind mEdge.path pathConstraints
            |> Option.map (fun c -> [c.source, mEdge.source
                                     c.target, fst mEdge.lastEdge])

        let returnNodes = Set.ofList query.basic.nodes
        let mKEdgesToNodes mKEdges = 
            List.collect (fun p -> 
                match Map.tryFind p.path mKEdges.currEdges with 
                | None   -> [] 
                | Some e -> [p.source, e.source; p.target, fst e.lastEdge]
                ) query.basic.pathConstraints

        let nodesSet = Set.ofList query.basic.nodes

        matchEdges graph query.basic
        |> List.map (mKEdgesToNodes 
                     >> List.filter (fst >> flip Set.contains nodesSet)
                     >> Map.ofList)
