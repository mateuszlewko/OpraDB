namespace OpraDB

open OpraDB.LangTypes
open FSharpx

module RegexNFA = 

    type State = 
        | Matched 
        | Constraint of NodeConstraint * NextState 
        | Any of NextState
        | Pass of NextState
    and NextState = {
        next            : State
        mutable nextAlt : State option
    }

    module NextState = 
        let create next = {next = next; nextAlt = None}
        let createAlt next nextAlt = {next = next; nextAlt = Some nextAlt}

    module State = 
        open NextState

        let ofRegExp = 
            let rec build continueWith =
                function 
                | EpsilonExp         -> Matched
                | AnyExp             -> create continueWith |> Any
                | NodeExp constr     -> create continueWith 
                                        |> curry Constraint constr
                | ConcatExp (e1, e2) -> build (build Matched e2) e1
                | StarExp e          -> 
                    let curr = create continueWith 
                    let state = Pass curr
                    curr.nextAlt <- build state e |> Some
                    state
                | UnionExp (e1, e2)  -> 
                    createAlt (build continueWith e1) (build continueWith e2)
                    |> Pass
            
            build Matched