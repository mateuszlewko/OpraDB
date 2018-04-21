namespace OpraDB 

open OpraDB.QueryData
open OpraDB.AST
open OpraDB.CommonUtils

open Hekate
open FSharpx
open FSharpx.Option

module Labelling = 

    let private toLiteral labelName labels =
        Map.tryFind labelName labels
        |> Option.getOrElse Null

    let private nodeVarToIx kEdges =
        let getNode onPath getter =     
            Map.tryFind onPath kEdges
            |> Option.map (fun mEdge -> getter mEdge.lastEdge)
            
        function 
        | CurrNodeVar u -> getNode u fst
        | NextNodeVar v -> getNode v snd

    let value kEdges graph (ID label) = 
        let ofVar  = nodeVarToIx kEdges
        let orNull = Option.getOrElse Null
        
        let nodeLabelling node =
            node 
            >>= flip Graph.Nodes.tryFind graph
            |> Option.map (snd >> toLiteral label)
            |> orNull
        
        let edgeLabelling u v =
            maybe {
                let! u    = ofVar u
                let! v    = ofVar v
                let! edge = Graph.Edges.tryFind u v graph
                return toLiteral label (thr3 edge)
            } |> orNull

        function
        | [u; v]    -> edgeLabelling u v
        | [nodeVar] -> ofVar nodeVar |> nodeLabelling
        | other     -> Null