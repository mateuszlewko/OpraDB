namespace OpraDB

open OpraDB.Data
open OpraDB.LangTypes
open OpraDB.RegexNFA
open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =

    type MatchedEdge = {
            source    : int Node
            lastEdge  : int Edge
            nfaStates : (State list * Identifier list) list
        }

    module MatchedEdge =
        let create nfaStates node = { source    = node
                                      lastEdge  = -1, node
                                      nfaStates = nfaStates
                                    }

    /// Move to states reachable within one transition in single nfa
    /// for a given edge
    let private moveEdge graph mNode =
        let outEdges = Graph.Nodes.outward (snd mNode.lastEdge) graph
                       |> Option.defaultValue []

        /// Move to states reachable within one transition in single nfa
        /// for a given edge
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

        /// Move all states in single nfa
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

    let matchingNodes (graph : Graph) (query : Query) =
        let nfaStates = List.map (fun (e, ids) -> [State.ofRegExp e], ids)
                           query.regularConstraints
        let mNodes = Graph.Nodes.toList graph
                     |> List.map (fst >> MatchedEdge.create nfaStates)

        let checkMatched node =
            node.nfaStates |> List.forall (fst >> List.exists ((=) Matched))

        let rec bfs mNodes =
            let nodesMatched, rest =
                List.collect (moveEdge graph) mNodes
                |> List.partition checkMatched

            printfn "Matched nodes:\n %A" nodesMatched
            bfs rest

        List.collect (moveEdge graph) mNodes |> bfs