namespace OpraDB

open OpraDB.LangTypes
open FSharpx

module RegexNFA =

    [<NoComparison>]
    type State =
        | Matched
        | Constraint of NodeConstraint * Transition
        | Any of Transition
        | Empty of Transition
    and [<NoComparison; NoEquality>] Transition = {
        next            : State
        mutable nextAlt : State option
    }

    module Transition =
        let create next = {next = next; nextAlt = None}
        let createAlt next nextAlt = {next = next; nextAlt = Some nextAlt}

    module State =
        open Transition

        let ofRegExp =
            let rec build continuation =
                function
                | EpsilonExp         -> Matched
                | AnyExp             -> create continuation |> Any
                | NodeExp constr     -> create continuation
                                        |> curry Constraint constr
                | ConcatExp (e1, e2) -> build (build continuation e2) e1
                | StarExp e          ->
                    let curr = create continuation
                    let state = Empty curr
                    curr.nextAlt <- build state e |> Some
                    state
                | UnionExp (e1, e2)  ->
                    createAlt (build continuation e1) (build continuation e2)
                    |> Empty

            build Matched