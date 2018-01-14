namespace OpraDB

open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.Data

open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =

    type NFA = Transition list * Identifier list

    type MatchedEdge = {
            path      : Identifier
            source    : int Node
            lastEdge  : int Edge
            nfaStates : NFA list
        }

    module MatchedEdge =
        let create path nfaStates node = { source    = node
                                           path      = path
                                           lastEdge  = -1, node
                                           nfaStates = nfaStates }

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

        //  TODO: Use logary
        // printfn "outward edges for %d: %A" currNode
        //     (List.map (fun (u, v, _) -> u, v) outEdges)

        // map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa.
        outEdges |> List.choose moveEdge

    /// Get nodes that match regular constraints in a given query.
    let matchEdges (graph : Graph) (query : Query) =
        let nfaStates : Map<_, NFA list> =
            let allPathsIDs =
                List.collect snd query.regularConstraints
                |> List.distinct |> List.map (fun i -> i, []) |> Map.ofList

            query.regularConstraints
            |> List.fold (fun m (e, ids) ->
                let nfa = [State.ofRegExp e], ids
                List.fold (fun m id ->
                    Map.add id (nfa :: Map.find id m) m) m ids
                ) allPathsIDs

        let mNodes =
            let allNodes = Graph.Nodes.toList graph
            let createEdges (path, nfaStates) =
                List.map (fst >> MatchedEdge.create path nfaStates) allNodes

            nfaStates |> Map.toList |> List.collect createEdges

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
        |> List.distinctBy (fun n -> n.source, fst n.lastEdge)