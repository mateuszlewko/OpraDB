namespace OpraDB

open OpraDB.LangTypes
open OpraDB.RegexNFA
open OpraDB.Data

open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =

    type MatchedEdge = {
            source    : int Node
            lastEdge  : int Edge
            nfaStates : (Transition list * Identifier list) list
        }

    module MatchedEdge =
        let create nfaStates node = { source    = node
                                      lastEdge  = -1, node
                                      nfaStates = nfaStates
                                    }

    /// Get all outward edges of a given node, which have at least one
    /// state in every NFA they belong to.
    let private moveEdge graph mNode =
        /// Move to states reachable within one transition in single NFA
        /// for a given edge. Skips empty states.
        let rec moveState edge ids transition =
            let rec skipEmpty t =
                let moveFurther = Option.map skipEmpty
                                  >> Option.defaultValue []
                if t.state = Empty
                then moveFurther t.next @ moveFurther t.nextAlt
                else [t]

            let ok t = [t.next; t.nextAlt]
                       |> List.choose (Option.map skipEmpty) |> List.concat

            match transition.state with
            | Matched           -> []
            | Constraint constr ->
                if NodeConstraints.check edge graph constr
                then ok transition
                else []
            | Any   -> ok transition
            | Empty -> skipEmpty transition
                       |> List.collect (moveState edge ids)

        /// Move every state in a single NFA.
        let moveNFA edge (states, ids) =
            List.collect (moveState edge ids) states, ids

        /// Try to move every state in all NFAs for a given edge.
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

        let currNode = snd mNode.lastEdge
        let sinkNode = currNode, -1, Map.empty
        let outEdges = Graph.Nodes.outward currNode graph
                       |> Option.map (List.cons sinkNode)
                       |> Option.getOrElse []

        // printfn "outward edges for %d: %A" currNode
        //     (List.map (fun (u, v, _) -> u, v) outEdges)

        // map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa.
        outEdges |> List.choose moveEdge

    /// Get nodes that match regular constraints in a given query.
    let matchingNodes (graph : Graph) (query : Query) =
        let nfaStates = List.map (fun (e, ids) -> [State.ofRegExp e], ids)
                           query.regularConstraints
        let mNodes = Graph.Nodes.toList graph
                     |> List.map (fst >> MatchedEdge.create nfaStates)

        let checkMatched node =
            node.nfaStates
            |> List.forall (fst >> List.exists (fun t -> t.state = Matched))

        let rec bfs result mNodes =
            if List.isEmpty mNodes
            then result
            else
                let nextNodes          = List.collect (moveEdge graph) mNodes
                let nodesMatched, rest = List.partition checkMatched nextNodes

                // printfn "Nodes:\n %A" mNodes
                // printfn "Matched nodes:\n %A" nodesMatched
                // printfn "Rest of nodes:\n %A" rest
                bfs (nodesMatched @ result) nextNodes

        List.collect (moveEdge graph) mNodes |> bfs []
        |> List.map (fun n -> n.source, fst n.lastEdge) |> List.distinct