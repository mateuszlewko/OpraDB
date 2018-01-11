namespace OpraDB

open OpraDB.Data
open OpraDB.LangTypes
open OpraDB.RegexNFA
open FSharpx.Prelude
open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =

    type MatchedEdge = {
            source    : int Node
            lastEdge  : int Edge
            nfaStates : (State list * Identifier list) list
        }

    let moveNode mNode graph =
        let outEdges = Graph.Nodes.outward (snd mNode.lastEdge) graph
                       |> Option.defaultValue []

        let rec moveState edge ids  =
            let ok t = match t.nextAlt with
                       | Some s -> [t.next; s]
                       | None   -> [t.next]
            function
            | Matched -> []
            | Constraint (constr, transition) ->
                if NodeConstraints.check edge graph constr ids
                then ok transition
                else []
            | Any transition   -> ok transition
            | Empty transition ->
                moveState edge ids transition.next
                    @ (Option.map (moveState edge ids) transition.nextAlt
                       |> Option.defaultValue [])

        let moveNFA edge (states, ids) =
            List.collect (moveState edge ids) states, ids

        /// Try to move each state in every nfa for a given edge
        let moveEdge edge =
            let rec collectStates nfaStates =
                function
                | []           -> Some nfaStates
                | states::rest ->
                    match moveNFA edge states with
                    | [], _ -> None
                    | nfa   -> collectStates (nfa::nfaStates) rest

            collectStates [] mNode.nfaStates
            |> Option.map (fun nfaStates ->
                            { mNode with
                                lastEdge  = (fun (u, v, _) -> u, v) edge
                                nfaStates = nfaStates
                            })
        // map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa
        outEdges |> List.choose moveEdge

    // let matchingNodes (graph : Graph) (query : Query) =
    //     let nfaStates = List.map (fun (e, ids) -> State.ofRegExp e, ids)
    //                        query.regularConstraints
    //     List.
        // let nodeStates node =
        //     List.fold
        // ()