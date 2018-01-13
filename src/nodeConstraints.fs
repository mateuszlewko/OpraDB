namespace OpraDB

open OpraDB.LangTypes
open OpraDB.Data
open Hekate
open FSharpx

module NodeConstraints =

    let getOperator =
        function
        | Leq -> (<=)
        | Le  -> (<)
        | Geq -> (>=)
        | Ge  -> (>)
        | Eq  -> (=)
        | Neq -> (<>)

    let labellingValue name labels =
        match Map.tryFind name labels with
        | None -> IntLiteral 0
          // TODO: Find a better way to handle this case
        | Some (IntVal i)    -> IntLiteral i
        | Some (StringVal s) -> StringLiteral s

    /// Checks whether node constraint is satisfied for a given edge (u -> v)
    let check ((u, v, edgeLabels)) graph (NodeConstraint (lhs, op, rhs)) =
        let op l r = getOperator op l r

        let valueOfLabelling (ID name) vars =
            match List.tryFind (fun v -> v <> CurrNodeVar 1
                                      && v <> NextNodeVar 1) vars with
            | Some n ->
                let n = match n with
                        | CurrNodeVar v -> sprintf "@%d" v
                        | NextNodeVar v -> sprintf "@'%d" v

                printfn "WARNING: Labelling contains reference to node other than current one: %s. Edge %d -> %d." n u v
                IntLiteral 0
            | None ->
                let nodeLabelling node =
                    Graph.Nodes.tryFind node graph
                    |> Option.map (snd >> labellingValue name)
                    // TODO: Find a better way to handle node not found
                    |> Option.defaultValue (IntLiteral 0)

                match vars with
                | [CurrNodeVar 1; NextNodeVar 1] ->
                    labellingValue name edgeLabels
                | [CurrNodeVar 1] -> nodeLabelling u
                | [NextNodeVar 1] -> nodeLabelling v
                | other           ->
                    printfn "WARNING: Unsupported labelling %A. Edge %d -> %d."
                        other u v
                    IntLiteral 0

        let rec pred lhs rhs =
            match lhs, rhs with
            | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            | Labelling (name, vars), rhs ->
                pred (valueOfLabelling name vars) rhs
            | lhs, Labelling (name, vars) ->
                pred lhs (valueOfLabelling name vars)
            | lhs, rhs ->
                printfn "WARNING: Mismatched types of operands %A and %A. Edge %d -> %d."
                    lhs rhs u v
                false

        pred lhs rhs