namespace OpraDB 

open OpraDB.QueryData
open OpraDB.AST
open OpraDB.CommonUtils
open OpraDB.ValueExpression

open Hekate
open FSharpx
open FSharpx.Option

module Labelling = 

    exception WrongLetTypeException of LetBody

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

    let letValue letQueriesRes kEdges graph labelling letExp args : Literal = 
        let rec onBody = 
            function 
            | Value v -> if List.length args <> List.length letExp.args
                         then failwith "wrong number of args"
                         else eval letQueriesRes kEdges labelling 
                                (renameVarsFrom letExp.args args v)
            | Query q -> let (ID name) = letExp.name
                         ResultOfQuery (name, args) |> Value |> onBody
            | other   -> raise (WrongLetTypeException other)
        
        onBody letExp.body 

    let rec value letExps letQueriesRes kEdges graph (ID label) = 
        match Map.tryFind label letExps with 
        | Some letExp -> 
            let labelling = value letExps letQueriesRes kEdges graph
            letValue letQueriesRes kEdges graph labelling letExp
        | None        ->
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
            | [nodeVar] -> 
                let res = ofVar nodeVar |> nodeLabelling
                res
            | other     -> Null