namespace OpraDB

open OpraDB.Graph
open OpraDB.LangTypes
open OpraDB.RegexNFA
open OpraDB.NodeConstraints
open FSharpx.Prelude
open FSharpx.Collections 

module RegularConstraints =
    type MatchedNode = {
            source   : int 
            lastEdge : Edge 
            nfsStates : Transition list list
        }

    let matchingNodes (graph : Graph) (query : Query) =
        let nfaStates = List.map (fst >> State.ofRegExp) query.regularConstraints
        // let nodeStates node = 
        //     List.fold 
        ()