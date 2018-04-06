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
// open Microsoft.Z3.Array

open System.Numerics

module Arr = Collections.Array

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

    type AttrDelta = { path : Identifier; attr: Identifier; delta: int }

    let existsSolution constraints (cyclesDeltas : Map<Identifier * Identifier, int> list) =
        let cycleCnt = List.length cyclesDeltas
        /// Value of alpha-i represents  how many times to traverse i-th cycle
        let alphas   = Arr.init cycleCnt (sprintf "alpha-%d" >> Int)
        /// All mappings from (path, attribute) to delta for a given cycle
        let deltas   = Array.ofList cyclesDeltas
        
        /// Mapping from (path, attribute) to Int expression representing 
        /// sum of alpha_i * delta_i for a given (path, attribute)
        let attrAlphas = 
            let attrIndexes indexes (i : int, cycle) = 
                Map.fold (fun indexes key _ -> MultiMap.add key i indexes) 
                         indexes cycle

            let constructExpr id indexes = 
                let indexes = Array.ofSeq indexes
                let getAlpha i = 
                    let delta = Map.find id deltas.[i]
                    alphas.[i] * (BigInteger delta)

                Array.map getAlpha indexes
                |> Array.fold (+) (IntVal 0I)

            Array.indexed deltas 
            |> Array.fold attrIndexes Map.empty
            |> Map.map constructExpr 

        let rec evalOperand =
            function 
            | IntALiteral i -> BigInteger i |> IntVal
            | Add (l, r)    -> evalOperand l + evalOperand r 
            | Mult (l, r)   -> evalOperand l * evalOperand r
            | SumBy (p, l)  -> Map.tryFind (p, l) attrAlphas
                               |> Option.defaultValue (IntVal 0I) 

        let evalOperator : Operator -> Int -> Int -> _ = 
            function 
            | Eq  -> (=. )
            | Neq -> (<>.)
            | Leq -> (<=.)
            | Geq -> (>=.)
            | Ge  -> (>. )
            | Le  -> (<. )

        let evalConstraint (ArithmeticConstraint (lhs, op, rhs)) =
            (evalOperator op) (evalOperand lhs) (evalOperand rhs)

        let constraintsExpr = Array.ofList constraints 
                              |> Array.map evalConstraint 

        let result = Z3.Solve (And constraintsExpr)
        
        printfn "result %A" result
        result.ToFSharpOption () |> Option.isSome

    let inequalitiesSatisfied mKEdges predecessors attrs graph constraints = 
        let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
        let cyclesAtrrsDelta  = Graph.Utils.allSimpleCycles subGraph
                                |> List.map (attributesDelta graph attrs)
   
        existsSolution constraints cyclesAtrrsDelta
   