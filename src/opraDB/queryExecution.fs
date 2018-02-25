namespace OpraDB

open OpraDB.RegularConstraints
open OpraDB.QueryData
open OpraDB.AST
open FSharpx
open FSharpx.Collections

module QueryExecution =

    let execute graph query =
        let pathConstraints =
            List.map (fun c -> c.path, c) query.pathConstraints |> Map.ofList
        let getStartEndNodes (mEdge : MatchedEdge) =
            Map.tryFind mEdge.path pathConstraints
            |> Option.map (fun c -> [c.source, mEdge.source
                                     c.target, fst mEdge.lastEdge])

        let returnNodes = Set.ofList query.nodes

        matchEdges graph query
        |> List.map (fun me -> Map.valueList me.currEdges 
                               |> List.map MatchedEdge.basicInfo)
        |> printfn "%A"
        // |>! List.map ^ fun e -> printfn "Matched edges %A" (e.path, e.source, fst e.lastEdge)
        // |> List.choose getStartEndNodes
        // |> List.map (List.filter (fst >> (flip Set.contains returnNodes)))
        // |> List.filter (List.isEmpty >> not)