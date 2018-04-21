namespace OpraDB

open FSharpx.Option

open OpraDB.AST
open OpraDB.QueryData
open OpraDB.NodeConstraints
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

open Microsoft.Z3

module ArithmeticConstraints =

    let addNodeAttributes graph curr mKEdges  =
        let addValue (path, ID labelName) curr =
            Map.tryFind path mKEdges.currEdges
            >>= fun e -> Graph.Nodes.tryFind (fst e.lastEdge) graph
            >>= fun (_, labels) -> Map.tryFind labelName labels
            |> Option.map (opArith Add curr)
            |> Option.getOrElse curr
        Map.map addValue curr   
        
    let updateArithStates graph mKEdges =
        { mKEdges with arithStates = addNodeAttributes graph mKEdges.arithStates 
                                                       mKEdges }

    let rec private summedValueExprs =
        let rec get acc = 
            function
            | Sum v       -> v::acc
            | AC.Value va -> getVal acc va
        and getVal acc = 
            function 
            | ArithOp (l, _, r)
            | BoolOp  (l, _, r) -> getVal (getVal acc r) l
            | Ext     e         -> get acc e
            | other             -> acc
        get []

    let createArithStates query =
        List.collect summedValueExprs query.arithmeticConstraints 
        |> flip Seq.zip (Seq.repeat None)
        |> Map.ofSeq

    let attributesDelta graph attrs cycle = 
        let curr = Seq.map (fun a -> a, None) attrs |> Map.ofSeq
        List.fold (addNodeAttributes graph) curr cycle

    let findSolution constraints arithStates cyclesDeltas =
        // printfn "arith: %A" arithStates
        // printfn "deltas: %A" cyclesDeltas

        use ctx      = Context.create ()
        use solver   = Solver.create ctx
        let cycleCnt = List.length cyclesDeltas

        /// Value of alpha-i represents  how many times to traverse i-th cycle
        let alphas   = Array.init cycleCnt (fun i -> (ctx.MkIntConst (sprintf "alpha-%d" i)))
        /// All mappings from (path, attribute) to delta for a given cycle
        let deltas   = Array.ofList cyclesDeltas

        let mkConst (i : int) = ctx.MkAdd (ctx.MkInt 0, ctx.MkInt i)

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
                    ctx.MkMul (alphas.[i], ctx.MkInt delta)

                let add l r = ctx.MkAdd (l, r)
                let arithVal = Map.tryFind id arithStates 
                               |> Option.getOrElse 0

                Array.map getAlpha indexes
                |> Array.fold add (mkConst arithVal)

            Array.indexed deltas 
            |> Array.fold attrIndexes Map.empty
            |> Map.map constructExpr 

        let rec evalOperand =
            function 
            | IntALiteral i -> mkConst i
            | Add (l, r)    -> ctx.MkAdd (evalOperand l, evalOperand r)
            | Mult (l, r)   -> ctx.MkMul (evalOperand l, evalOperand r)
            | SumBy (p, l)  -> Map.tryFind (p, l) attrAlphas 
                               |> Option.defaultValue (mkConst 0) 

        let evalOperator lhs rhs = 
            function 
            | Eq  -> ctx.MkEq       (lhs, rhs)
            | Neq -> ctx.MkDistinct (lhs, rhs)
            | Leq -> ctx.MkLe       (lhs, rhs)
            | Geq -> ctx.MkGe       (lhs, rhs)
            | Ge  -> ctx.MkGt       (lhs, rhs)
            | Le  -> ctx.MkLt       (lhs, rhs)
            | And -> ctx.MkAnd      (lhs, rhs)
            | Or  -> ctx.MkOr       (lhs, rhs)

        let evalConstraint (ArithmeticConstraint (lhs, op, rhs)) =
            evalOperator (evalOperand lhs) (evalOperand rhs) op

        let constraintsExpr = Array.ofList constraints 
                              |> Array.map evalConstraint 

        let exprs = Array.map (fun a -> ctx.MkGe (a, ctx.MkInt 0)) alphas
                    |> Array.append constraintsExpr

        solver.Add exprs
        Solver.check solver

    let foundSolution = 
        function
        | Solution _ -> true
        | _          -> false

    let existsSolution constraints arithStates cyclesDeltas = 
        findSolution constraints arithStates cyclesDeltas
        |> foundSolution

    let inequalitiesSatisfied mKEdges predecessors graph constraints = 
        let attrs = Map.keys mKEdges.arithStates |> List.ofSeq

        let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
        let cyclesAtrrsDelta  = Graph.Utils.allSimpleCycles subGraph
                                |> List.map (attributesDelta graph attrs)
   
        findSolution constraints mKEdges.arithStates cyclesAtrrsDelta 
        |> foundSolution
   