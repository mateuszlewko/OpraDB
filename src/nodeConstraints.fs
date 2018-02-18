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
        let indexToPath = List.indexed ids |> Map.ofList
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
            | lhs, rhs ->
                // printfn "WARNING: Mismatched types of operands %A and %A. Edge %d -> %d."
                false

        pred lhs rhs

    /// Checks whether node constraint is satisfied for a given edge (u -> v)
    // let check (u, v, edgeLabels) graph (NodeConstraint (lhs, op, rhs)) =
    //     let op l r = getOperator op l r

    //     let valueOfLabelling (ID name) vars =
    //         match List.tryFind (fun v -> v <> CurrNodeVar 1
    //                                   && v <> NextNodeVar 1) vars with
    //         | Some n ->
    //             let n = match n with
    //                     | CurrNodeVar v -> sprintf "@%d" v
    //                     | NextNodeVar v -> sprintf "@'%d" v

    //             // printfn "WARNING: Labelling contains reference to node other than current one: %s. Edge %d -> %d." n u v
    //             IntLiteral 0
    //         | None ->
    //             let nodeLabelling node =
    //                 Graph.Nodes.tryFind node graph
    //                 |> Option.map (snd >> labellingValue name)
    //                 // TODO: Find a better way to handle node not found
    //                 |> Option.defaultValue (IntLiteral 0)

    //             match vars with
    //             | [CurrNodeVar 1; NextNodeVar 1] ->
    //                 labellingValue name edgeLabels
    //             | [CurrNodeVar 1] -> nodeLabelling u
    //             | [NextNodeVar 1] -> nodeLabelling v
    //             | other           ->
    //                 // printfn "WARNING: Unsupported labelling %A. Edge %d -> %d."
    //                 //     other u v
    //                 IntLiteral 0

    //     let rec pred lhs rhs =
    //         match lhs, rhs with
    //         | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
    //         | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
    //         | Labelling (name, vars), rhs ->
    //             pred (valueOfLabelling name vars) rhs
    //         | lhs, Labelling (name, vars) ->
    //             pred lhs (valueOfLabelling name vars)
    //         | lhs, rhs ->
    //             // printfn "WARNING: Mismatched types of operands %A and %A. Edge %d -> %d."
    //             //     lhs rhs u v
    //             false

    //     pred lhs rhs