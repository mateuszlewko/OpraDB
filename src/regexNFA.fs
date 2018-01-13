namespace OpraDB

open OpraDB.LangTypes

module RegexNFA =

    type State =
        | Matched
        | Constraint of NodeConstraint
        | Any
        | Empty

    [<NoComparison; NoEquality>]
    type Transition = {
        state           : State
        next            : Transition option
        mutable nextAlt : Transition option
    }

    module Transition =
        let matched = {state = Matched; next = None; nextAlt = None}
        let create state next =
            {state = state; next = Some next; nextAlt = None}
        let createAlt state next nextAlt =
            {state = state; next = Some next; nextAlt = Some nextAlt}

    module State =
        open Transition

        let ofRegExp =
            let rec build continuation =
                function
                | EpsilonExp         -> matched
                | AnyExp             -> create Any continuation
                | NodeExp constr     -> create (Constraint constr) continuation
                | ConcatExp (e1, e2) -> build (build continuation e2) e1
                | StarExp e          ->
                    let curr = create Empty continuation
                    curr.nextAlt <- build curr e |> Some
                    curr
                | UnionExp (e1, e2)  ->
                    createAlt Empty (build continuation e1) (build continuation e2)

            build matched