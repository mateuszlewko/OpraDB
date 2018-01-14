namespace OpraDB

open OpraDB.RegularConstraints
open OpraDB.LangTypes
open FSharpx
// open OpraDB.Data
// open Hekate

module QueryExecution =

    let execute graph query =
        let returnNodes = Set.ofList query.nodes
        let pathConstraints =
            List.map (fun c -> c.path, c) query.pathConstraints |> Map.ofList
        let getStartEndNodes (mEdge : MatchedEdge) =
            Map.tryFind mEdge.path pathConstraints
            |> Option.map (fun c -> [c.source, mEdge.source;
                                     c.target, fst mEdge.lastEdge])

        matchEdges graph query
        |> List.choose getStartEndNodes
        |> List.map (List.filter (fst >> (flip Set.contains returnNodes)))
        |> List.filter (List.isEmpty >> not)