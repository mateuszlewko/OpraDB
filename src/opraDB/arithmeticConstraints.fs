namespace OpraDB

open FSharpx.Option

open OpraDB.AST
open OpraDB.QueryData
open OpraDB.Data
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

open Microsoft.Z3
open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3.Array

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

    let constraintToInequality (solver : Solver) 
                               (ArithmeticConstraint (l, op, r)) = 
        solver.Add ()

    let existsSolution constraints values = 
        // Create 3 integer variables
        let dog = Int("dog")
        let cat = Int("cat")
        let mouse = Int("mouse")

        let result = 
            Z3.Solve(dog >=. 1I,   // at least one dog
                     cat >=. 1I,   // at least one cat
                     mouse >=. 1I, // at least one mouse
                     // we want to buy 100 animals
                     dog + cat + mouse =. 100I,  
                     // We have 100 dollars (10000 cents):
                     // dogs cost 15 dollars (1500 cents), 
                     //   cats cost 1 dollar (100 cents), and 
                     //   mice cost 25 cents
                     1500I * dog + 100I * cat + 25I * mouse =. 10000I)
        
        printfn "result %A" result

    let inequalitiesSatisfied mKEdges predecessors attrs graph = 
        let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
        let cycles            = Graph.Utils.allSimpleCycles subGraph
        let cyclesAtrrsDelta  = List.map (attributesDelta graph attrs) cycles

        true
   