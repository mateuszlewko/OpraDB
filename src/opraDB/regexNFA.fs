namespace OpraDB

open OpraDB.AST
open OpraDB.ValueExpression

module RegexNFA =

    type State =
        | Matched
        | Constraint of NodeConstraint
        | Any
        | Empty

    [<CustomComparison; CustomEquality>]
    type Transition = {
        state           : State
        next            : Transition option
        mutable nextAlt : Transition option
        tid             : int
    } with 
        override this.Equals other =  
            match other with 
            | :? Transition as rhs -> this.tid = rhs.tid
            | _                    -> false 

        override this.GetHashCode() = this.tid.GetHashCode ()

        interface System.IComparable with 
            member this.CompareTo other =  
                match (other : obj) with 
                |  :? Transition as rhs -> compare this.tid rhs.tid
                | _ -> invalidArg "other" 
                                  "cannot compare value of different types" 

    module Transition =
        let matched tid = { tid = tid; state = Matched
                          ; next = None; nextAlt = None }
       
        let create tid state next =
            { tid = tid; state = state; next = Some next; nextAlt = None }
       
        let createAlt tid state next nextAlt =
            { tid = tid; state = state; next = Some next
            ; nextAlt = Some nextAlt }

    let rec private renameRegExp letExpArgs passedArgs exp = 
        let rename = renameRegExp letExpArgs passedArgs 
        match exp with  
        | NodeExp n -> renameVarsFrom letExpArgs passedArgs n 
                       |> NodeExp
        | ConcatExp (l, r) -> ConcatExp (rename l, rename r)
        | UnionExp  (l, r) -> UnionExp (rename l, rename r)
        | StarExp s        -> StarExp (rename s)
        | LetCall (ID name as id, vars) ->
            let pArgs = List.map NodeVariable.identifier passedArgs
            let mp    = List.zip letExpArgs passedArgs |> Map.ofList
            let map nodeV v = Map.tryFind v mp |> Option.defaultValue (nodeV v)
            let vars = vars
                       |> List.map (function
                                    | CurrNodeVar v -> map CurrNodeVar v
                                    | NextNodeVar v -> map NextNodeVar v)
            LetCall (id, vars)
        | AnyExp                  -> AnyExp
        
    module State =
        open Transition                                      

        let ofRegExp letExps regExp =
            let mutable currID = 0
            let getID () = let res = currID
                           currID <- currID + 1
                           res
            let (!!) f = f ()

            let rec build continuation =
                function
                | AnyExp             -> create !!getID Any continuation
                | NodeExp constr     -> create !!getID (Constraint constr) 
                                               continuation
                | ConcatExp (e1, e2) -> build (build continuation e2) e1
                | StarExp e          ->
                    let curr = create  !!getID Empty continuation
                    curr.nextAlt <- build curr e |> Some
                    curr
                | UnionExp (e1, e2)  ->
                    createAlt !!getID Empty (build continuation e1) 
                                               (build continuation e2)
                | RE.LetCall (ID name, args) -> 
                    match Map.tryFind name letExps with 
                    | None        -> NodeExp (Labelling (ID name, args))
                                     |> build continuation
                                     //failwithf "unbound name: %A" name 
                    | Some letExp ->
                        match letExp.body with 
                        | Value v   -> renameVarsFrom letExp.args args v
                                       |> NodeExp |> build continuation 
                        | Regular r -> renameRegExp letExp.args args r
                                       |> build continuation 
                        | other     -> failwithf "invalid type of %s in regular constraints" 
                                                 name

            build (matched !!getID) regExp