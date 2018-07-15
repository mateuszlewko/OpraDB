namespace OpraDB 

open OpraDB.AST
open OpraDB.QueryData
open FSharpx

module ValueExpression =

    type ValueType = IntT | BoolT | StringT | FloatT | NullT

    exception NotSupportedArithOpException of ArithOperator * Literal * Literal
    exception NotSupportedBoolOpException of BoolOperator * Literal * Literal
   
    let literalType = 
        function 
        | Int    _ -> IntT
        | Bool   _ -> BoolT 
        | Float  _ -> FloatT 
        | String _ -> StringT 
        | Null     -> NullT 

    type private AO = ArithOperator

    let rec evalArith op lhs rhs = 
        let inline get op =    
            match op with
            | Add    -> (+)
            | Sub    -> (-)
            | Mult   -> (*)
            | Div    -> (/)
            | AO.Max -> max
            | AO.Min -> min  

        let notSupp op l r = 
            NotSupportedArithOpException (op, l, r) 
                    
        match lhs, rhs with 
        | Null, _ | _, Null                -> Null
        | Int l, Int r                     -> (get op) l r |> Int
        | Float l, Float r                 -> (get op) l r |> Float
        | String l as ls, (String r as rs) -> match op with 
                                              | Add -> l + r |> String
                                              | op  -> notSupp op ls rs |> raise
        | String _ as s, other 
        | other, (String _ as s)  -> notSupp op s other |> raise
        | Float f as l, (Int r)   -> evalArith op l (Float (float r))
        | (Int l), (Float f as r) -> evalArith op (Float (float l)) r
        | Bool _ as b, other 
        | other , (Bool _ as b)   -> notSupp op b other |> raise

    let rec evalBool op lhs rhs = 
        let notSupp op l r = 
            NotSupportedBoolOpException (op, l, r) 
                    
        let inline get op l r = 
            match op with
            | Leq   -> l <= r
            | Le    -> l <  r
            | Geq   -> l >= r
            | Ge    -> l >  r
            | Eq   
            | Is    -> l =  r 
            | Neq
            | IsNot -> l <> r
            | op    -> notSupp op l r |> raise
            |> Bool

        let bGet l r =
            function
            | And   -> l && r
            | Or    -> l || r
            | Neq 
            | IsNot -> l <> r
            | Eq 
            | Is    -> l =  r
            | op    -> notSupp op (Bool l) (Bool r) |> raise
            >> Bool

        match lhs, rhs with 
        | Null, other | other, Null ->
            match op, other with 
            | Is, _ | IsNot, _  -> get op Null other 
            | And, Bool false   -> Bool false  
            | Or , Bool true    -> Bool true   
            | other             -> Null
        // same type expression
        | Int     _ as l , (Int    _ as r)         
        | (Float  _ as l), (Float  _ as r)   
        | (String _ as l), (String _ as r) -> get op l r 
        // bool expression
        | Bool l, Bool r         -> bGet l r op 
        // different types expression
        | other, (String _ as s) -> notSupp op s other |> raise
        | (String _ as s), other -> notSupp op s other |> raise
        | Float f as l, (Int r)  -> get op l (Float (float r))
        | Int l, (Float _ as r)  -> get op (Float (float l)) r
        // all other cases are unsupported
        | l, r -> notSupp op l r |> raise

    let private getNodesMapper kEdges = 
        let map = Map.map (fun _ (e : MatchedEdge) -> e.lastProperEdge) kEdges
        function 
        | CurrNodeVar i -> Map.tryFind i map |> Option.map fst
        | NextNodeVar i -> Map.tryFind i map |> Option.map snd

    let rec evalExt ext letQueriesRes kEdges labellingValue valExpr =
        let exp = evalExt ext letQueriesRes kEdges labellingValue

        match valExpr with 
        | Lit l                  -> l 
        | Labelling (ids, vars)  -> labellingValue ids vars 
        | ArithOp (lhs, op, rhs) -> let lhs, rhs = exp lhs, exp rhs
                                    evalArith op lhs rhs
        | BoolOp (lhs, op, rhs)  -> let lhs, rhs = exp lhs, exp rhs 
                                    evalBool op lhs rhs
        | ResultOfQuery (q, ids) -> 
            let results = Map.find q letQueriesRes
            let resSets = Lazy.force results 
            // printfn "res sets: %A" resSets 
            // let mapper = getNodesMapper kEdges
            let curr = List.choose (getNodesMapper kEdges)  ids
            // printfn "nodes map: %A" nodesMap 
            // printfn "ids: %A" ids
            // failwith ""
            Set.contains curr resSets 
            |> Bool
        | Ext e                  -> ext e

    let eval letQueriesRes = evalExt (fun () -> Null) letQueriesRes

    let rec renameVarsExt renameExt mapping valExpr = 
        let rename = renameVarsExt renameExt mapping

        match valExpr with  
        | Lit _ as l              -> l
        | Labelling (id, args)    -> Labelling (id, List.map mapping args)
        | ArithOp (l, op, r)      -> let l, r = rename l, rename r
                                     ArithOp (l, op, r)
        | BoolOp  (l, op, r)      -> let l, r = rename l, rename r
                                     BoolOp (l, op, r)
        | ResultOfQuery (q, args) -> ResultOfQuery (q, List.map mapping args)
        | Ext e                   -> Ext (renameExt e)

    let renameVars mapping = renameVarsExt id mapping

    let mappingOf mp = 
        function 
        | CurrNodeVar i -> CurrNodeVar (mp i)
        | NextNodeVar i -> NextNodeVar (mp i)

    let renameVarsFrom letExpArgs passedArgs = 
        let mp = List.map NodeVariable.identifier passedArgs
                 |> List.zip letExpArgs
                 |> Map.ofList
                 |> flip Map.find
               
        renameVars (mappingOf mp)