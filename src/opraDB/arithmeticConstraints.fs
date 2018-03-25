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

    let updateArithStates graph (mKEdges : MatchedKEdges) =
        let addValue (path, ID labelName) curr =
            Map.tryFind path mKEdges.currEdges
            >>= fun e -> Graph.Nodes.tryFind (fst e.lastEdge) graph
            >>= fun (_, labels) -> Map.tryFind labelName labels
            |> function Some (IntVal i) -> i + curr | _ -> curr     
        
        { mKEdges with arithStates = Map.map addValue mKEdges.arithStates }

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

    let restoreGraph preds mKEdges = 
        let rec restore node visited graph = 
            if Set.contains node visited
            then graph, visited
            else let visited = Set.add node visited 
                 match Map.tryFind node preds with 
                 | None     -> graph, visited
                 | Some pre -> 
                     Set.fold (fun (graph, vis) p -> 
                             let tryAdd n g =
                                 if Graph.Nodes.contains n g 
                                 then g else Graph.Nodes.add (n, ()) g
 
                             tryAdd p graph |> tryAdd node 
                             |> Graph.Edges.add (p, node, ()) 
                             |> restore p vis
                         ) (graph, visited) pre
        
        restore mKEdges Set.empty Graph.empty
