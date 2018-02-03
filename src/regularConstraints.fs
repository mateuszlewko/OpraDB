namespace OpraDB

open FSharpx.Functional

open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.Data

open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =

    /// All current states in a single NFA, along with
    /// id of paths applied to it
    type NFAState = Transition list * Identifier list

    type MatchedEdge = {
            path      : Identifier
            source    : int Node
            lastEdge  : int Edge
            nfaStates : NFAState list
        }

    let [<Literal>] NULL_NODE = -1

    module MatchedEdge =
        let create path nfaStates node = { source    = node
                                           path      = path
                                           lastEdge  = NULL_NODE, node
                                           nfaStates = nfaStates }
                                         
        let basicInfo e = e.path, e.source, fst e.lastEdge

    open MatchedEdge

    /// Get all outward edges of a given node, which have at least one
    /// state in every NFA they belong to.
    let private moveEdge graph mNode =
        /// Move to states reachable within one transition in single NFA
        /// for a given edge. Skips empty states.
        let rec moveState edge ids transition =
            /// Get all states after skipping all empty ones.
            let rec skipEmpty t =
                let moveFurther = Option.map skipEmpty >> Option.defaultValue []
               
                if t.state = Empty
                then moveFurther t.next @ moveFurther t.nextAlt
                else [t]

            /// Get all reachable states, skipping empty transitions.
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
        let sinkNode = currNode, NULL_NODE, Map.empty
        let outEdges = Graph.Nodes.outward currNode graph
                       |> Option.map (List.cons sinkNode)
                       |> Option.getOrElse []

        //  TODO: Use logary
        printfn "outward edges for %d: %A" currNode
            (List.map (fun (u, v, _) -> u, v) outEdges)

        // Map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa.
        outEdges |> List.choose moveEdge

    /// Get nodes that match regular constraints in a given query.
    let matchEdges (graph : Graph) (query : Query) =
        /// list of all NFAs for every path
        let nfaStates : Map<_, NFAState list> =
            let allPathsIDs =
                List.collect snd query.regularConstraints
                |> List.distinct |> List.map (fun i -> i, []) |> Map.ofList

            // build and append all NFAs
            query.regularConstraints
            |> flip List.fold allPathsIDs (
                fun m (e, ids) ->
                    let nfa      = [State.ofRegExp e], ids
                    let add m id = Map.add id (nfa :: Map.find id m) m
                    List.fold add m ids 
                ) 

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

                let mapInfo = List.map basicInfo
                printfn "Nodes:\n %A"         ^ mapInfo mNodes
                printfn "Matched nodes:\n %A" ^ mapInfo nodesMatched
                printfn "Rest of nodes:\n %A" ^ mapInfo rest
                bfs (nodesMatched @ result) nextNodes

        List.collect (moveEdge graph) mNodes |> bfs []
        |> List.distinctBy basicInfo