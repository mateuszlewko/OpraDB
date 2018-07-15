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

    let private addNodeAttributes letQueriesRes kEdges labellingValue 
                                  arithStates =
        let addValue (aggType, valueExpr) curr =
            /// Null values are treated as neutral for arithmetic constraints
            let rhs = 
                match eval letQueriesRes kEdges labellingValue valueExpr with 
                | Null  -> None 
                | other -> Some other

            // printfn "expr: %A has val: %A" valueExpr rhs
            match curr, rhs with 
            | Some v   , None 
            | None     , Some v   -> Some v 
            | Some curr, Some rhs -> 
                let op = match aggType with 
                         | Sum -> Add 
                         | Max -> ArithOperator.Max 
                         | Min -> ArithOperator.Min
                evalArith true op curr rhs |> Some
            | None     , None     -> None 

        Map.map addValue arithStates   
        
    let updateArithStates letExps letQueriesRes graph mKEdges =
        let labelling   = 
            Labelling.value letExps letQueriesRes mKEdges.currEdges graph
        let arithStates = 
            addNodeAttributes letQueriesRes mKEdges.currEdges labelling 
                mKEdges.arithStates
        { mKEdges with arithStates = arithStates }

    let rec private arithValueExprs env letExps =
        let rec get env acc = 
            function
            | Aggregate (op,  v) -> 
                let mp k = Map.tryFind k env |> Option.getOrElse k
                let mapping = mappingOf mp
                (op, renameVars mapping v)::acc
            | AC.Value va        -> getVal env acc va

        and getVal env acc = 
            function 
            | ArithOp (l, _, r)
            | BoolOp  (l, _, r)         -> getVal env (getVal env acc r) l
            | Ext     e                 -> get env acc e
            | Labelling (ID name, vars) ->
                match Map.tryFind name letExps with 
                | None        -> acc 
                | Some letExp -> 
                    let mp  = List.map NodeVariable.identifier vars
                              |> List.zip letExp.args
                              |> Map.ofList
                    let env = Map.union env mp 

                    match letExp.body with 
                    | Arith a -> acc @ (arithValueExprs env letExps a)
                    // | Value v -> getVal // ValueExpr probably wouldn't contain SUM (...)
                    | other  -> failwithf "call to %A is unsupported in arithmetic constraint" other
            | other -> acc

        get env []

    // let rec inlineLabellings letExps = 
    //     let doInline name vars = 
    //         match Map.tryFind name letExps with 
    //         | None        -> failwith "unbound name: %s" name
    //         | Some letExp -> 
    //             match letExp.body with 
    //             | Value ve ->  

    //     let rec inlineValue ext = 
    //         function 
    //         | Labelling (ID name, vars)

    //     let inlineArith = 
    //         function 
    //         | Value 
    //         | Sum _ as s -> s

    let createArithStates query letExps : ArithStates =
        query.arithmeticConstraints
        |> List.collect (arithValueExprs Map.empty letExps) 
        |> flip Seq.zip (Seq.repeat None)
        |> Map.ofSeq

    let private attributesDelta letExps letQueriesRes graph attrs cycle = 
        let curr = Seq.map (fun a -> a, None) attrs |> Map.ofSeq
       
        let folder state mKEdges = 
            let labelling = 
                Labelling.value letExps letQueriesRes mKEdges.currEdges graph
            addNodeAttributes letQueriesRes mKEdges.currEdges labelling state
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
        | op   -> 
            failwithf "Unexpected operator: %A in arithmetic constraint\n" op

    type EvalType = BoolT of BoolExpr | ArithT of ArithExpr

    let findSolution constraints letExps arithStates 
                     (cyclesDeltas : Map<_, Literal> list) =

        printfn "arith: %A" arithStates
        // printfn "deltas: %A" cyclesDeltas

        use ctx      = Context.create ()
        use solver   = Solver.create ctx
        let cycleCnt = List.length cyclesDeltas

        let name      = sprintf "alpha-%d"
        /// Value of alpha-i represents how many times to traverse i-th cycle
        let alphas    = Array.init cycleCnt (name >> ctx.MkIntConst )
        /// All mappings from ValueExpr to delta (Literal) of a given cycle
        let deltas    = Array.ofList cyclesDeltas
        /// Raise WrongTypeException for a literal
        let wrongT    = literalType >> WrongTypeException
        let arith0 () = ctx.MkInt 0 :> ArithExpr

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

                let add l r  = ctx.MkAdd (l, r)
                let arithVal = Map.tryFind (Sum, id) arithStates 
                               |> Option.flatten
                               |> Option.map ofLiteral
                               |> Option.getOrElse (arith0 ())

                Array.map getAlpha indexes
                |> Array.fold add arithVal

            Array.indexed deltas 
            |> Array.fold attrIndexes Map.empty
            |> Map.map constructExpr 

        let rec evalValue evalExt evalArith evalValueUnit env valExp =
            let evalValueWith = evalValue evalExt evalArith evalValueUnit 
            let evalValue     = evalValue evalExt evalArith evalValueUnit env

            match valExp with
            | ArithOp (l, op, r) -> match evalValue l, evalValue r with 
                                    | ArithT l, ArithT r -> 
                                        arithOp ctx l r op |> ArithT
                                    | _, _ -> failwith "wrong type"
            | Labelling (ID name, vars) -> 
                match Map.tryFind name letExps with 
                | None        -> failwithf "unbound name: %s" name
                | Some letExp -> 
                    /// rename vars
                    let mp  = List.map NodeVariable.identifier vars
                              |> List.zip letExp.args
                              |> Map.ofList
                    let env = Map.union env mp 
                    // printfn "new env: %A" env                  

                    match letExp.body with 
                    | Value v -> evalValueUnit env v
                    | Arith a -> evalArith env a
                    | other   -> failwithf "call to %A is unsupported in arithmetic constraint" other
            | BoolOp (l, op, r) -> match evalValue l, evalValue r with 
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
                                    
            | Ext a -> evalExt env a
            | Lit l -> ofLiteral l |> ArithT

        let rec evalArith env arith =
            match arith with
            | Aggregate (Sum, s) -> 
                // printfn "env: %A" env
                let mp k = Map.tryFind k env |> Option.getOrElse k
                let mapping = mappingOf mp
                let s = renameVars mapping s
                // printfn "sum of %A" s

                Map.tryFind s attrAlphas
                // FIXME: TODO: Maybe should be value from arithStates instead of 0?
                |> Option.getOrElse (Map.tryFind (Sum, s) arithStates
                                     |> Option.flatten
                                     |> Option.map ofLiteral
                                     |> Option.getOrElse (arith0 ()))
                |> ArithT
            | Aggregate (op, s)  -> 
                Map.tryFind (op, s) arithStates 
                |> Option.flatten
                |> Option.map ofLiteral
                |> Option.getOrElse (ctx.MkInt 0 :> ArithExpr)
                |> ArithT
            | AC.Value v         -> 
                let rec evalValueUnit env e = 
                    evalValue (fun _ () -> failwith "unexpected") evalArith 
                              evalValueUnit env e
                evalValue evalArith evalArith evalValueUnit env v
                    
        let toBool = function 
                     | BoolT b -> b 
                     | other   -> failwith "constr must be of bool type"

        let constraintsExpr = Array.ofList constraints 
                              |> Array.map (evalArith Map.empty >> toBool)
                               
        let exprs = alphas 
                    |> Array.map (fun a -> ctx.MkGe (a, ctx.MkInt 0)) 
                    |> Array.append constraintsExpr

        // Array.iter (fun (x : BoolExpr) -> printfn "=> %s" <| x.ToString()) exprs

        solver.Add exprs
        Solver.check solver

    let foundSolution = 
        function
        | Solution _ -> 
            printfn "FOUND solution"
            true
        | _          -> 
            printfn "NOT found solution"
            false

    let existsSolution constraints letExps arithStates cyclesDeltas = 
        findSolution constraints letExps arithStates cyclesDeltas
        |> foundSolution

    let inequalitiesSatisfied mKEdges letExps letQueriesRes predecessors graph =
        function
        | []          -> true 
        | constraints ->
            let summedAttrs = 
                Map.keys mKEdges.arithStates |> List.ofSeq
                |> List.filter (function (Sum, _) -> true | _ -> false)

            let subGraph, visited = Graph.Utils.restoreGraph predecessors mKEdges
            let cyclesAtrrsDelta  = 
                Graph.Utils.allSimpleCycles subGraph
                |> List.map (attributesDelta letExps letQueriesRes graph 
                                summedAttrs 
                             >> Map.toList
                             >> List.choose 
                                    (function ((Sum, x), Some v) -> Some (x, v)
                                                   | _           -> None)
                             >> Map.ofList                                                
                            )
                // |> Map.choose (function (Sum, x) -> konst (Some x) 
                //                        | _       -> konst None)                            
       
            findSolution constraints letExps mKEdges.arithStates 
                         cyclesAtrrsDelta 
            |> foundSolution
   