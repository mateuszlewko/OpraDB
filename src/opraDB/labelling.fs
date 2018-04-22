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

    let letValue kEdges graph (labelling : Identifier -> NodeVariable list -> Literal) letExp args = 
        match letExp.body with 
        | Value v -> if List.length args <> List.length letExp.args
                     then failwith "wrong number of args"
                     else let mp = List.map NodeVariable.identifier args
                                   |> List.zip letExp.args  
                                   |> Map.ofList
                                   |> flip Map.find
                                   
                          let mapping = 
                              function 
                              | CurrNodeVar i -> CurrNodeVar (mp i)
                              | NextNodeVar i -> NextNodeVar (mp i)

                          eval labelling (renameVars mapping v)
        | other   -> raise (WrongLetTypeException other)

    let rec value letExps kEdges graph (ID label) = 
        match Map.tryFind label letExps with 
        | Some letExp -> 
                         let labelling : Identifier -> NodeVariable list -> Literal = value letExps kEdges graph
                         letValue kEdges graph labelling letExp
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
            | [(u : NodeVariable); v]    -> edgeLabelling u v
            | [nodeVar] -> ofVar nodeVar |> nodeLabelling
            | other     -> Null