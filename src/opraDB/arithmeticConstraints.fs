namespace OpraDB

open OpraDB.AST
open OpraDB.QueryData
open OpraDB.NodeConstraints
open OpraDB.ValueExpression
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

open Microsoft.Z3

module ArithmeticConstraints =

    let private addNodeAttributes labellingValue arithStates  =
        let addValue valueExpr curr =
            /// Null values are treated as neutral for arithmetic constraints
            let rhs = match eval labellingValue valueExpr with 
                      | Null  -> None 
                      | other -> Some other

            match curr, rhs with 
            | Some v   , None 
            | None     , Some v   -> Some v 
            | Some curr, Some rhs -> evalArith Add curr rhs |> Some
            | None     , None     -> None 

        Map.map addValue arithStates   
        
    let updateArithStates letExps graph mKEdges =
        let labelling   = Labelling.value letExps mKEdges.currEdges graph
        let arithStates = addNodeAttributes labelling mKEdges.arithStates
        { mKEdges with arithStates = arithStates }

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

    let createArithStates query : ArithStates =
        List.collect summedValueExprs query.arithmeticConstraints 
        |> flip Seq.zip (Seq.repeat None)
        |> Map.ofSeq

    let private attributesDelta letExps graph attrs cycle = 
        let curr = Seq.map (fun a -> a, None) attrs |> Map.ofSeq
       
        let folder state mKEdges = 
            let labelling = Labelling.value letExps mKEdges.currEdges graph
            addNodeAttributes labelling state
        List.fold folder curr cycle

    let private cmpOp (ctx : Context) lhs rhs = 
        function 
        | Eq  
        | Is    -> ctx.MkEq       (lhs, rhs)
        | Neq 
        | IsNot -> ctx.MkDistinct (lhs, rhs)
        | Leq   -> ctx.MkLe       (lhs, rhs)
        | Geq   -> ctx.MkGe       (lhs, rhs)
        | Ge    -> ctx.MkGt       (lhs, rhs)
        | Le    -> ctx.MkLt       (lhs, rhs)
        | And 
        | Or    -> failwith "cmp operator required"
        // | And   -> ctx.MkAnd      (lhs, rhs)
        // | Or    -> ctx.MkOr       (lhs, rhs)

    let private arithOp (ctx : Context) lhs rhs = 
        function 
        | Add  -> ctx.MkAdd (lhs, rhs)
        | Sub  -> ctx.MkSub (lhs, rhs)
        | Mult -> ctx.MkMul (lhs, rhs)
        | Div  -> ctx.MkDiv (lhs, rhs)

    type EvalType = BoolT of BoolExpr | ArithT of ArithExpr

    let findSolution constraints arithStates 
                     (cyclesDeltas : Map<_, Literal> list) =

        // printfn "arith: %A" arithStates
        // printfn "deltas: %A" cyclesDeltas

        use ctx      = Context.create ()
        use solver   = Solver.create ctx
        let cycleCnt = List.length cyclesDeltas

        let name     = sprintf "alpha-%d"
        /// Value of alpha-i represents how many times to traverse i-th cycle
        let alphas   = Array.init cycleCnt (name >> ctx.MkIntConst )
        /// All mappings from ValueExpr to delta (Literal) of a given cycle
        let deltas   = Array.ofList cyclesDeltas

        // let mkConst (i : int) = ctx.MkAdd (ctx.MkInt 0, ctx.MkInt i)
        let add0 x = ctx.MkAdd (ctx.MkInt 0, x) |> ArithT
        let wrongT = literalType >> WrongTypeException

        let ofLiteral =
            function
            | Null    -> ctx.MkInt 0           :> ArithExpr
            | Int i   -> ctx.MkInt i           :> ArithExpr
            | Float f -> ctx.MkReal (string f) :> ArithExpr
            | other   -> wrongT other |> raise

        /// Mapping from ValueExpr to ArithExpr expression representing 
        /// sum of alpha_i * delta_i for a given SUM ValueExpr
        let attrAlphas = 
            let attrIndexes indexes (i : int, cycle) = 
                Map.fold (fun indexes key _ -> MultiMap.add key i indexes) 
                         indexes cycle

            let constructExpr id indexes = 
                let indexes = Array.ofSeq indexes
                let getAlpha i = 
                    match Map.find id deltas.[i] with 
                    | Null    -> ctx.MkMul (alphas.[i], ctx.MkInt 0)
                    | Int   x -> ctx.MkMul (alphas.[i], ctx.MkInt x)
                    | Float x -> ctx.MkMul (alphas.[i], ctx.MkReal (string x))
                    | other   -> wrongT other |> raise

                let add l r = ctx.MkAdd (l, r)
                let arithVal = Map.tryFind id arithStates 
                               |> Option.flatten
                               |> Option.map ofLiteral
                               |> Option.getOrElse (ctx.MkInt 0 :> ArithExpr)

                Array.map getAlpha indexes
                |> Array.fold add arithVal

            Array.indexed deltas 
            |> Array.fold attrIndexes Map.empty
            |> Map.map constructExpr 

        let rec evalArith =
            function
            | Sum s      -> Map.tryFind s attrAlphas
                            |> Option.getOrElse (ctx.MkInt 0 :> ArithExpr) 
                            |> ArithT
            | AC.Value v -> evalValue v
        and evalValue = 
            function 
            | ArithOp (l, op, r) -> match evalValue l, evalValue r with 
                                    | ArithT l, ArithT r -> 
                                        arithOp ctx l r op |> ArithT
                                    | _, _ -> failwith "wrong type"
            | Labelling _        -> failwith "unsupported"
            | BoolOp (l, op, r)  -> match evalValue l, evalValue r with 
                                    | BoolT l , BoolT r  -> 
                                        match op with 
                                        | And   -> ctx.MkAnd (l, r)
                                        | Or    -> ctx.MkOr (l, r)
                                        | Is 
                                        | Eq    -> ctx.MkEq (l, r)
                                        | IsNot 
                                        | Neq   -> ctx.MkNot (ctx.MkEq (l, r))
                                        | other -> failwith "wrong type"
                                        |> BoolT
                                    
                                    | ArithT l, ArithT r -> cmpOp ctx l r op
                                                            |> BoolT
                                    | l, r -> 
                                        failwithf "mismatched types %A %A" l r
                                    
            | Ext a -> evalArith a
            | Lit l -> ofLiteral l |> ArithT
        
        let toBool = function 
                     | BoolT b -> b 
                     | other   -> failwith "constr must be of bool type"

        let constraintsExpr = Array.ofList constraints 
                              |> Array.map (evalArith >> toBool)
                              
        let exprs = alphas 
                    |> Array.map (fun a -> ctx.MkGe (a, ctx.MkInt 0)) 
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

    let inequalitiesSatisfied mKEdges letExps predecessors graph =
        function
        | []          -> true 
        | constraints ->
            let attrs = Map.keys mKEdges.arithStates |> List.ofSeq

            let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
            let cyclesAtrrsDelta  = Graph.Utils.allSimpleCycles subGraph
                                    |> List.map (attributesDelta letExps graph attrs 
                                                 >> Map.choose (konst id))
       
            findSolution constraints mKEdges.arithStates cyclesAtrrsDelta 
            |> foundSolution
   