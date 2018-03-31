namespace OpraDB

open FSharpx.Option

open OpraDB.AST
open OpraDB.QueryData
open OpraDB.Data
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

module ArithmeticConstraints =

    let addNodeAttributes graph curr mKEdges  =
        let addValue (path, ID labelName) curr =
            Map.tryFind path mKEdges.currEdges
            >>= fun e  -> Graph.Nodes.tryFind (fst e.lastEdge) graph
            >>= fun (_, labels) -> Map.tryFind labelName labels
            |> function Some (IntVal i) -> i + curr
                      | _               -> curr   
        Map.map addValue curr   
        
    let updateArithStates graph mKEdges =
        { mKEdges with arithStates = addNodeAttributes graph mKEdges.arithStates 
                                                       mKEdges }

    let private allPathsAndLabellings (ArithmeticConstraint (l, _, r)) =
        let rec fromOp acc = 
            function
            | SumBy (p, l)             -> (p, l)::acc
            | Add (l, r) | Mult (l, r) -> fromOp (fromOp acc l) r
            | _                        -> acc

        fromOp (fromOp [] l) r

    let createArithStates query =
        List.collect allPathsAndLabellings query.arithmeticConstraints 
        |> List.distinct |> flip Seq.zip (Seq.initInfinite (konst 0)) 
        |> Map.ofSeq

    let private constrSatisfied mKEdges (ArithmeticConstraint (l, op, r)) = 
        let rec evalOperand =
            function 
            | IntALiteral i -> i 
            | Add (l, r)    -> evalOperand l + evalOperand r 
            | Mult (l, r)   -> evalOperand l * evalOperand r
            | SumBy (p, l)  -> Map.tryFind (p, l) mKEdges.arithStates
                               |> Option.defaultValue 0        

        (getOperator op) (evalOperand l) (evalOperand r)
                            
    let satisfied mKEdges arithConstrs = 
        List.forall (constrSatisfied mKEdges) arithConstrs

    //                4 <------ 6
    // 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7
    // 
    let attributesDelta graph attrs cycle = 
        let curr = List.map (fun a -> a, 0) attrs |> Map.ofList
        List.fold (addNodeAttributes graph) curr cycle