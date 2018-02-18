namespace OpraDB

open OpraDB.AST
open OpraDB.Data
open OpraDB.QueryData
open OpraDB.CommonUtils

open Hekate
open FSharpx
open FSharpx.Option

module NodeConstraints =

    let getOperator =
        function
        | Leq -> (<=)
        | Le  -> (<)
        | Geq -> (>=)
        | Ge  -> (>)
        | Eq  -> (=)
        | Neq -> (<>)

    let private labellingValue name labels =
        match Map.tryFind name labels with
        | None               -> IntLiteral 0
        | Some (IntVal    i) -> IntLiteral i
        | Some (StringVal s) -> StringLiteral s

    let private nodeVarTo indexToPath kEdges =
        let getNode pathID getter =     
            maybe {
                let! p = Map.tryFind pathID indexToPath
                let! mEdge = Map.tryFind p kEdges
                return getter mEdge.lastEdge
            }

        function 
        | CurrNodeVar u -> getNode u fst
        | NextNodeVar v -> getNode v snd

    let checkKEdges kEdges graph (NodeConstraint (lhs, op, rhs)) ids =
        let indexToPath = List.mapi (fun i x -> i + 1, x) ids |> Map.ofList
        let op l r      = getOperator op l r
        let ofVar       = nodeVarTo indexToPath kEdges

        let valueOfLabelling (ID label) =
            let nodeLabelling node =
                (snd >> labellingValue label) <!> Graph.Nodes.tryFind node graph
            
            let edgeLabelling u v =
                maybe {
                    let! u    = ofVar u
                    let! v    = ofVar v
                    let! edge = Graph.Edges.tryFind u v graph
                    return labellingValue label (thr3 edge)
                }

            function
            | [u; v]    -> edgeLabelling u v
            | [nodeVar] -> ofVar nodeVar >>= nodeLabelling
            | other     -> None
            >> (Option.defaultValue (IntLiteral 0))

        let rec pred lhs rhs =
            match lhs, rhs with
            | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            | Labelling (name, vars), rhs ->
                pred (valueOfLabelling name vars) rhs
            | lhs, Labelling (name, vars) ->
                pred lhs (valueOfLabelling name vars)
            | lhs, rhs -> false

        pred lhs rhs