namespace OpraDB

open OpraDB.AST

module RegexNFAData =

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