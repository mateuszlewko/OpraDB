namespace OpraDB

open OpraDB.Data
open OpraDB.QueryData
open OpraDB.AST
open OpraDB.CommonUtils

open Hekate
open FSharpx
open FSharpx.Option

module NodeConstraints =

    let private getBoolOp =
        function
        | Leq -> (<=)
        | Le  -> (<)
        | Geq -> (>=)
        | Ge  -> (>)
        | Eq 
        | Is  -> (=)
        | Neq -> (<>)
        | And -> (&&)
        | Or  -> (||)

    let inline private getArithOp x =    
        function
        | Add  -> (+) x
        | Sub  -> (-) x
        | Mult -> (*) x
        | Div  -> (/) x


    exception NotSupportedArithOpException of ArithOperator * Literal * Literal
    exception NotSupportedBoolOpException of BoolOperator * Literal * Literal

    let private bToI = function true -> 1 | false -> 0
    let private iTob = function 0 -> false | _ -> true

    let toFloat = 
        function 
        | Int   i -> Float (float i)
        | Bool  b -> Float (float (bToI b))
        | other   -> other

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
        | other, (String _ as s) -> notSupp op s other |> raise
        | Float f as l, r        -> opArith op l (toFloat r)
        | l, (Float f as r)      -> opArith op (toFloat l) r
        | Bool l, r              -> opArith op (bToI l |> Int) r
        | l, Bool r              -> opArith op l (bToI r |> Int) 

    let rec opBool op lhs rhs = 
        let inline get op =    
            match op with
            | Leq -> (<=)
            | Le  -> (< )
            | Geq -> (>=)
            | Ge  -> (> )
            // | Eq 
            // | Is  -> (= )
            // | Neq -> (<>)
            // | And -> (&&)
            // | Or  -> (||)

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
        | other, (String _ as s) -> notSupp op s other |> raise
        | Float f as l, r        -> opArith op l (toFloat r)
        | l, (Float f as r)      -> opArith op (toFloat l) r
        | Bool l, r              -> opArith op (bToI l |> Int) r
        | l, Bool r              -> opArith op l (bToI r |> Int) 



    let rec valueExpr ext labellingVal =
        let exp = valueExpr ext labellingVal

        function 
        | Lit l                  -> l 
        | Labelling (ids, vars)  -> labellingVal ids vars 
        | ArithOp (lhs, op, rhs) -> let lhs, rhs = exp lhs, exp rhs
                                    opArith op lhs rhs
        | BoolOp (lhs, op, rhs)  -> Null
        | Ext e                  -> ext e

    let private toLiteral labelName labels =
        Map.tryFind labelName labels
        |> Option.getOrElse Null
        // | None               -> IntLiteral 0
        // | Some (IntVal    i) -> IntLiteral i
        // | Some (StringVal s) -> StringLiteral s

    let private nodeVarTo kEdges =
        let getNode onPath getter =     
            maybe {
                let! mEdge = Map.tryFind onPath kEdges
                return getter mEdge.lastEdge
            }

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

        let rec pred =
            function
            | String s -> 
            // match lhs, rhs with
            // // | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            // // | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            // | Labelling (name, vars), rhs -> pred (labellingVal name vars) rhs
            // | lhs, Labelling (name, vars) -> pred lhs (labellingVal name vars)
            // | lhs, rhs -> false

        pred lhs rhs