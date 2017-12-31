namespace OpraDB

open OpraDB.Graph
open OpraDB.LangTypes
open OpraDB.RegexNFA
open OpraDB
open FSharpx.Option
open FSharpx.Collections 
open Hekate

module RegularConstraints =
    type MatchedNode = {
            source    : int 
            lastEdge  : Edge 
            nfaStates : NextState list list
        }

    let matchingNodes (graph : Graph) (query : Query) =
        let nfaStates = List.map (fst >> State.ofRegExp) query.regularConstraints
        let nodeStates node = 
            let rec checkState = 
                function 
                | Matched -> Some [] 
                | Constraint (constr, next) -> 
                    if NodeConstraints.check node constr 
                    then Some [next]
                    else None
                | Any next -> Some [next] 
                | Pass state ->
                    let next = [checkState state.next
                                state.nextAlt >>= checkState] 
                               |> List.choose id |> List.concat
                    if List.isEmpty next
                    then None 
                    else Some next
                    
            List.fold 
        ()