namespace OpraDB

open FSharpx.Option

open OpraDB.AST
open OpraDB.QueryData
open OpraDB.Data
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open FSharpx.Prelude
open Hekate

open Microsoft.Z3
// open Microsoft.Z3.Bool
// open Microsoft.Z3.Int

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

    // let constraintToInequality (solver : Solver)
    //                            (ArithmeticConstraint (l, op, r)) = 
    //     solver.Add ()

    type AttrDelta = { path : Identifier; attr: Identifier; delta: int }

    let mutable cnt = 0

    let private existsSolutionInner constraints cyclesDeltas ctx =
        
        // System.GC.KeepAlive (ctx2)
        // Native.Z3_del_context Gs.globalCtx
        // Gs.globalCtx <- ctx2
        // System.GC.KeepAlive (Gs)
        use solver : Solver =  Solver.create ctx
        // System.GC.KeepAlive (solver)
        // solver.Push()
        cnt <- cnt + 1

        let cycleCnt = List.length cyclesDeltas
        // let ctx =context
        /// Value of alpha-i represents  how many times to traverse i-th cycle
        let alphas   = Arr.init cycleCnt (fun i -> (ctx.MkIntConst (sprintf "%d-alpha-%d" cnt i)))
        /// All mappings from (path, attribute) to delta for a given cycle
        let deltas   = Array.ofList cyclesDeltas

        // printfn "alphas: %A" alphas
        // printfn "deltas: %A" deltas
        
        /// Mapping from (path, attribute) to Int expression representing 
        /// sum of alpha_i * delta_i for a given (path, attribute)
        let attrAlphas = 
            let attrIndexes indexes (i : int, cycle) = 
                Map.fold (fun indexes key _ -> MultiMap.add key i indexes) 
                         indexes cycle

            let constructExpr id indexes = 
                let indexes = Array.ofSeq indexes
                let getAlpha i = 
                    let delta : int = Map.find id deltas.[i]
                    
                    System.Console.WriteLine (sprintf "i: %d" i)
                    System.Console.Out.Flush ()
                    System.Console.WriteLine (sprintf "a: %O" alphas.[i])
                    System.Console.Out.Flush ()
                    System.Console.WriteLine (sprintf "d: %d" delta)
                    System.Console.Out.Flush ()

                    ctx.MkMul (alphas.[i], ctx.MkInt delta)

                let rec fold = 
                    function 
                    | []    -> ctx.MkAdd (ctx.MkInt 0, ctx.MkInt 0)
                    | [x]   -> x
                    | x::y::xs -> fold (ctx.MkAdd(x, y)::xs)

                Array.map getAlpha indexes
                |> Array.toList
                |> fold

            Array.indexed deltas 
            |> Array.fold attrIndexes Map.empty
            |> Map.map constructExpr 

        // let (>>*) x f = f x; x

        let rec evalOperand =
            function 
            | IntALiteral i -> ctx.MkAdd (ctx.MkInt 0, ctx.MkInt i)
            | Add (l, r)    -> ctx.MkAdd (evalOperand l, evalOperand r)
            | Mult (l, r)   -> ctx.MkMul (evalOperand l, evalOperand r)
            | SumBy (p, l)  -> match Map.tryFind (p, l) attrAlphas with 
                               | Some x -> x
                               | None   -> ctx.MkAdd (ctx.MkInt 0, ctx.MkInt 0)
                            //    |> Option.defaultValue (IntVal 0I) 
                            //    >>*  (printfn "op: %O")

        let evalOperator lhs rhs = 
            function 
            | Eq  -> ctx.MkEq (lhs, rhs)
            | Neq -> ctx.MkDistinct (lhs, rhs)
            | Leq -> ctx.MkLe (lhs, rhs)
            | Geq -> ctx.MkGe (lhs, rhs)
            | Ge  -> ctx.MkGt (lhs, rhs)
            | Le  -> ctx.MkLt (lhs, rhs)

        let evalConstraint (ArithmeticConstraint (lhs, op, rhs)) =
            evalOperator (evalOperand lhs) (evalOperand rhs) op

        let constraintsExpr = Array.ofList constraints 
                              |> Array.map evalConstraint 

        // let s = Solver ()
        // let ctx = (Gs.context())
        // use ctx = new Context ()
        // // Gs.context
        // let a = 

        let result : SolveResult = 
            let exprs = Array.map (fun a -> ctx.MkGe (a, ctx.MkInt 0)) alphas
                        |> Array.append constraintsExpr
                        // |> Array.map (function BoolExpr e -> e)

            solver.Add exprs
            Solver.check solver
            // Z3.Solve (And exprs)
            
            // Z3.Solve (And constraintsExpr)
        // true
        // printfn "result %O" result
        // let g = (Gs.context())
        // let solver = g.MkSimpleSolver ()
        // solver.Push ()
        // let s = solver.Check () 
        // let ss = Solver.check solver
        // solver.p
        match result with 
        | Solution s -> 
            printfn "%A" s
            true 
        | _          -> false


    let existsSolution constraints cyclesDeltas =
        use ctx = Context.create ()
        // Gs.globalCtx <- Some ctx
        let res = existsSolutionInner constraints cyclesDeltas ctx
        // Gs.globalCtx <- None
        // System.GC.Collect ()
        // ctx.Dispose ()
        res


    let inequalitiesSatisfied mKEdges predecessors attrs graph constraints = 
        let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
        let cyclesAtrrsDelta  = Graph.Utils.allSimpleCycles subGraph
                                |> List.map (attributesDelta graph attrs)
   
        existsSolution constraints cyclesAtrrsDelta
   