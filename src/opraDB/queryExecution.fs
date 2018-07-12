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
        
        // let getStartEndNodes (mEdge : MatchedEdge) =
        //     Map.tryFind mEdge.path pathConstraints
        //     |> Option.map (fun c -> [c.source, mEdge.source
        //                              c.target, fst mEdge.lastEdge])

        let returnNodes = Set.ofList query.basic.nodes
        let mKEdgesToNodes mKEdges = 
            List.collect (fun p -> 
                match Map.tryFind p.path mKEdges.currEdges with 
                | None   -> [] 
                | Some e -> [p.source, e.source; p.target, fst e.lastGoodEdge]
                ) query.basic.pathConstraints
            |> List.distinct

        let nodesSet = query.basic.nodes |> List.map NodeMatched.node 
                       |> List.distinct |> Set.ofList 

        let letExps  = query.letExps 
                       |> List.map (fun l -> let (ID name) = l.name in name, l) 
                       |> Map.ofList

        matchEdges graph letExps query.basic
        |> List.map (mKEdgesToNodes 
                     >> List.filter (fst >> flip Set.contains nodesSet)
                     >> Map.ofList)
