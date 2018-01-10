namespace OpraDB

open OpraDB.Data
open OpraDB.LangTypes
open OpraDB.RegexNFA
open OpraDB.NodeConstraints
open FSharpx.Prelude
open FSharpx.Collections 
open Hekate

module RegularConstraints =

    type MatchedNode = {
            source    : int Node 
            lastEdge  : int Edge
            nfaStates : State list list
        }

    let matchingNodes (graph : Graph) (query : Query) =
        let nfa = List.map (fun (e, ids) -> State.ofRegExp e, ids) 
                           query.regularConstraints
        // let nodeStates node = 
        //     List.fold 
        ()