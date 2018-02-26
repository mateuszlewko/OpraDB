namespace OpraDB

open OpraDB.RegularConstraints
open OpraDB.QueryData
open OpraDB.AST
open FSharpx
open FSharpx.Collections
open FSharpx.Option

module QueryExecution =

    let matchedNodes graph (query : Query) =
        let pathConstraints =
            List.map (fun c -> c.path, c) query.pathConstraints |> Map.ofList
        
        let getStartEndNodes (mEdge : MatchedEdge) =
            Map.tryFind mEdge.path pathConstraints
            |> Option.map (fun c -> [c.source, mEdge.source
                                     c.target, fst mEdge.lastEdge])

        let returnNodes = Set.ofList query.nodes
        let mKEdgesToNodes mKEdges = 
            List.collect (fun p -> 
                match Map.tryFind p.path mKEdges.currEdges with 
                | None   -> [] 
                | Some e -> [p.source, e.source; p.target, fst e.lastEdge]
                ) query.pathConstraints

        let nodesSet = Set.ofList query.nodes

        matchEdges graph query
        |> List.map (mKEdgesToNodes 
                     >> List.filter (fst >> flip Set.contains nodesSet)
                     >> Map.ofList)

        // |> printfn "%A"
        // |>! List.map ^ fun e -> printfn "Matched edges %A" (e.path, e.source, fst e.lastEdge)
        // |> List.choose getStartEndNodes
        // |> List.map (List.filter (fst >> (flip Set.contains returnNodes)))
        // |> List.filter (List.isEmpty >> not)