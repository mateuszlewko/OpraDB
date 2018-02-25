namespace OpraDB

open OpraDB.AST
open OpraDB.Data
open OpraDB.QueryData
open OpraDB.CommonUtils

open Hekate
open FSharpx
open FSharpx.Option

module NodeConstraints =

    let private toLiteral labelName labels =
        match Map.tryFind labelName labels with
        | None               -> IntLiteral 0
        | Some (IntVal    i) -> IntLiteral i
        | Some (StringVal s) -> StringLiteral s

    let private nodeVarTo kEdges =
        let getNode onPath getter =     
            maybe {
                let! mEdge = Map.tryFind onPath kEdges
                return getter mEdge.lastEdge
            }

        function 
        | CurrNodeVar u -> getNode u fst
        | NextNodeVar v -> getNode v snd

    let checkKEdges kEdges graph (NodeConstraint (lhs, op, rhs)) =
        // let indexToPath = List.mapi (fun i x -> i + 1, x) ids |> Map.ofList
        let op l r      = getOperator op l r
        let ofVar       = nodeVarTo kEdges

        let labellingVal (ID label) =
            let nodeLabelling node =
                (snd >> toLiteral label) <!> Graph.Nodes.tryFind node graph
            
            let edgeLabelling u v =
                maybe {
                    let! u    = ofVar u
                    let! v    = ofVar v
                    let! edge = Graph.Edges.tryFind u v graph
                    return toLiteral label (thr3 edge)
                }

            function
            | [u; v]    -> edgeLabelling u v
            | [nodeVar] -> ofVar nodeVar >>= nodeLabelling
            | other     -> None
            >> Option.defaultValue (IntLiteral 0)

        let rec pred lhs rhs =
            match lhs, rhs with
            | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            | Labelling (name, vars), rhs -> pred (labellingVal name vars) rhs
            | lhs, Labelling (name, vars) -> pred lhs (labellingVal name vars)
            | lhs, rhs -> false

        pred lhs rhs