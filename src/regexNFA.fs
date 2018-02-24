namespace OpraDB

open OpraDB.AST

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
                match other with 
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

    module State =
        open Transition

        let ofRegExp =
            let mutable currID = 0
            let getID () = let res = currID
                           currID <- currID + 1
                           res

            let rec build continuation =
                function
                | AnyExp             -> create (getID ()) Any continuation
                | NodeExp constr     -> create (getID ()) (Constraint constr) 
                                                continuation
                | ConcatExp (e1, e2) -> build (build continuation e2) e1
                | StarExp e          ->
                    let curr = create  (getID ()) Empty continuation
                    curr.nextAlt <- build curr e |> Some
                    curr
                | UnionExp (e1, e2)  ->
                    createAlt (getID ()) Empty (build continuation e1) 
                                               (build continuation e2)

            build (matched (getID ()))