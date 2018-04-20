namespace OpraDB

// open OpraDB.Data
open OpraDB.QueryData
open OpraDB.AST
open OpraDB.CommonUtils

open Hekate
open FSharpx
open FSharpx.Option

module NodeConstraints =

    // let private getBoolOp =
    //     function
    //     | Leq -> (<=)
    //     | Le  -> (< )
    //     | Geq -> (>=)
    //     | Ge  -> (> )
    //     | Eq 
    //     | Is  -> (= )
    //     | Neq 
    //     | NotIs -> (<>)
    //     | And   -> (&&)
    //     | Or    -> (||)

    // let inline private getArithOp x y =    
    //     function
    //     | Add  -> (+) x y 
    //     | Sub  -> (-) x y
    //     | Mult -> (*) x y
    //     | Div  -> (/) x y
    
    type ValueType = IntT | BoolT | StringT | FloatT | NullT

    let literalType = 
        function 
        | Int _    -> IntT
        | Bool _   -> BoolT 
        | Float _  -> FloatT 
        | String _ -> StringT 
        | Null     -> NullT 

    exception NotSupportedArithOpException of ArithOperator * Literal * Literal
    exception NotSupportedBoolOpException of BoolOperator * Literal * Literal
    exception WrongTypeException of ValueType 

    // let private bToI = function true -> 1 | false -> 0
    // let private iTob = function 0 -> false | _ -> true

    // let toFloat = 
    //     function 
    //     | Int   i -> Float (float i)
    //     // | Bool  b -> Float (float (bToI b))
    //     | other   -> other

    let rec opArith op lhs rhs = 
        let inline get op =    
            match op with
            | Add  -> (+)
            | Sub  -> (-)
            | Mult -> (*)
            | Div  -> (/)

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
        | Float f as l, (Int r)   -> opArith op l (Float (float r))
        | (Int l), (Float f as r) -> opArith op (Float (float l)) r
        | Bool _ as b, other 
        | other , (Bool _ as b)   -> notSupp op b other |> raise
        // | Bool l, r              -> opArith op (bToI l |> Int) r
        // | l, Bool r              -> opArith op l (bToI r |> Int) 

    let rec opBool op lhs rhs = 
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

        match lhs, rhs with 
        | Null, other | other, Null ->
            match op, other with 
            | Is, _ | IsNot, _  -> get op Null other 
            | And, Bool false   -> Bool false  
            | Or , Bool true    -> Bool true   
            | other             -> Null

        | Int _ as l     , (Int _ as r)         
        | (Float _ as l) , (Float _ as r)   
        | (String _ as l), (String _ as r) -> get op l r 
        
        | Bool l, Bool r         -> bGet l r op |> Bool
        | other, (String _ as s) -> notSupp op s other |> raise
        | (String _ as s), other -> notSupp op s other |> raise
        | Float f as l, (Int r)  -> get op l (Float (float r))
        | Int l, (Float _ as r)  -> get op (Float (float l)) r
        
        | l, r -> notSupp op l r |> raise

    let rec valueExpr ext labellingVal =
        let exp = valueExpr ext labellingVal

        function 
        | Lit l                  -> l 
        | Labelling (ids, vars)  -> labellingVal ids vars 
        | ArithOp (lhs, op, rhs) -> let lhs, rhs = exp lhs, exp rhs
                                    opArith op lhs rhs
        | BoolOp (lhs, op, rhs)  -> let lhs, rhs = exp lhs, exp rhs 
                                    opBool op lhs rhs
        | Ext e                  -> ext e

    let private toLiteral labelName labels =
        Map.tryFind labelName labels
        |> Option.getOrElse Null
        // | None               -> IntLiteral 0
        // | Some (IntVal    i) -> IntLiteral i
        // | Some (StringVal s) -> StringLiteral s

    let private nodeVarTo kEdges =
        let getNode onPath getter =     
            Map.tryFind onPath kEdges
            |> Option.map (fun mEdge -> getter mEdge.lastEdge)
            
        function 
        | CurrNodeVar u -> getNode u fst
        | NextNodeVar v -> getNode v snd

    let checkKEdges kEdges graph nodeConstr =
        // let indexToPath = List.mapi (fun i x -> i + 1, x) ids |> Map.ofList
        // let op l r      = getOperator op l r

        let ofVar  = nodeVarTo kEdges
        let orNull = Option.getOrElse Null

        let labellingVal (ID label) =
            let nodeLabelling node =
                node 
                >>= flip Graph.Nodes.tryFind graph
                |> Option.map (snd >> toLiteral label)
                |> orNull
            
            let edgeLabelling u v =
                maybe {
                    let! u    = ofVar u
                    let! v    = ofVar v
                    let! edge = Graph.Edges.tryFind u v graph
                    return toLiteral label (thr3 edge)
                } |> orNull

            function
            | [u; v]    -> edgeLabelling u v
            | [nodeVar] -> ofVar nodeVar |> nodeLabelling
            | other     -> Null

            // >> Option.defaultValue (IntLiteral 0)

        // let rec eval =
        //     function
        //     | String s -> 
            // match lhs, rhs with
            // // | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            // // | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            // | Labelling (name, vars), rhs -> pred (labellingVal name vars) rhs
            // | lhs, Labelling (name, vars) -> pred lhs (labellingVal name vars)
            // | lhs, rhs -> false

        // pred lhs rhs

        match valueExpr (konst Null) labellingVal nodeConstr with 
        | Bool res -> res 
        | Null     -> false
        | other    -> raise (WrongTypeException (literalType other))
        