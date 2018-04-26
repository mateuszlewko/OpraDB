namespace OpraDB.Shell

open OpraDB.Data
open FSharp.Data
open FSharpx
open FSharpx.Collections
open OpraDB.AST

module JsonImport = 

    exception ParsingErrorException of string

    let private parseGraph str = 
        let pExn str = raise (ParsingErrorException str)
        let parseProperty name label = 
            match label with 
            | JsonValue.Number i  -> Int (int i)
            | JsonValue.Float f   -> Float f
            | JsonValue.String s  -> String s
            | JsonValue.Boolean b -> Bool b
            | t                   -> pExn (sprintf "Unexpected property type: %A." t)

        let parseNode = 
            function
            | JsonValue.Record properties ->
                let properties = Map.ofArray properties

                match Map.tryFind "_id" properties with 
                | Some (JsonValue.Number id) -> 
                    int id, Map.map parseProperty properties
                | _ -> pExn "Node is missing '_id' field."
            | _ -> pExn "Node is expected to be a record."

        let parseEdge = 
            function
            | JsonValue.Record properties ->
                let properties = Map.ofArray properties
                let prop p     = Map.tryFind p properties

                match prop "_from", prop "_to" with 
                | Some (JsonValue.Number from), Some (JsonValue.Number to') -> 
                    int from, int to', Map.map parseProperty properties
                | _ -> pExn "Edge is missing '_from' or '_to' field."
            | _ -> pExn "Edge is expected to be a record."

        match JsonValue.Parse str with 
        | JsonValue.Record properties ->
            let properties = Map.ofArray properties 
            let prop p     = Map.tryFind p properties

            match prop "edges", prop "nodes" with 
            | Some (JsonValue.Array edges), Some (JsonValue.Array nodes) -> 
               let map f = Array.map f >> Array.toList
               map parseNode nodes, map parseEdge edges
            | _ -> pExn "Missing 'nodes' or edges field."
        | _ -> pExn "Expected json record on top level." 

    let importGraph str = parseGraph str |> uncurry buildGraph 
