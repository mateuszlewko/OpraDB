namespace OpraDB

open OpraDB.RegularConstraints
open OpraDB.QueryData
open OpraDB.AST
open FSharpx
open FSharpx.Collections

module QueryExecution =

    let rec matchedNodes graph (query : Query) =
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
                | Some e -> [p.source, e.source; p.target, fst e.lastProperEdge]
                ) query.basic.pathConstraints
            |> List.distinct

        let nodesSet = query.basic.nodes |> List.map NodeMatched.node 
                       |> List.distinct |> Set.ofList 

        let letExps  = query.letExps 
                       |> List.map (fun l -> let (ID name) = l.name in name, l) 
                       |> Map.ofList

        let letQueriesResults = 
            letExps
            |> Map.choose 
                (fun _ l -> match l.body with 
                            | Query q -> Some (l.args, q)
                            | _       -> None) 
            |> createResultsMapForLetQueries graph query.letExps

        matchEdges graph letExps letQueriesResults query.basic
        |> List.map (mKEdgesToNodes 
                     >> List.filter (fst >> flip Set.contains nodesSet)
                     >> Map.ofList)
        |> List.distinct                     

    and createResultsMapForLetQueries graph letExps queries =
        let matchedNodes q = matchedNodes graph { letExps = letExps; basic = q } 

        let argsToNodes args res = List.map (fun arg -> Map.find arg res) args
        let solutions args q     = matchedNodes q |> List.map (argsToNodes args) 
                                                  |> Set.ofList
                                
        queries |> Map.map (fun _ (args, q) -> lazy solutions args q)
