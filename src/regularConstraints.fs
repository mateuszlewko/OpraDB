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
        }

    type KEdges = Map<Identifier, MatchedEdge>

    type MatchedKEdges = {
            edges     : KEdges
            nfaStates : NFAState list
        }

    let [<Literal>] NULL_NODE = -1

    module MatchedEdge =
        let create path node = { source    = node
                                 path      = path
                                 lastEdge  = NULL_NODE, node }
                                         
        let basicInfo e = e.path, e.source, fst e.lastEdge

    open MatchedEdge

    // TODO: Move it to some utils
    let rec cartesian =
        function
        | []   -> []
        | [h]  -> List.fold (fun acc elem -> [elem]::acc) [] h
        | h::t ->
            List.fold (fun cacc celem ->
                (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
            ) [] (cartesian t)

    let private basicEdge (u, v, _) = u, v

    /// Get all outward edges of a given node, which have at least one
    /// state in every NFA they belong to.
    let private nextKEdges graph (mKEdges : MatchedKEdges list) =
        /// Move to states reachable within one transition in single NFA
        /// for a given kEdges. Skips empty states.
        let rec moveState kEdges ids transition =
            /// Get all states after skipping all empty ones.
            let rec skipEmpty t =
                let moveFurther = Option.map skipEmpty >> Option.defaultValue []
               
                if t.state = Empty
                then moveFurther t.next @ moveFurther t.nextAlt
                else [t]

            /// Get all reachable states, skipping empty transitions.
            let ok t = [t.next; t.nextAlt]
                       |> List.choose ^ Option.map skipEmpty |> List.concat

            match transition.state with
            | Matched           -> []
            | Constraint constr ->
                if NodeConstraints.checkEdges kEdges graph constr
                then ok transition
                else []
            | Any   -> ok transition
            | Empty -> skipEmpty transition
                       |> List.collect (moveState kEdges ids)

        /// Move every state in a single NFA.
        let moveNFA edges (states, ids) =
            List.collect (moveState edges ids) states, ids

        /// Try to move every state in all NFAs for a given k-edge.
        let moveKEdges kEdges =
            let rec collectStates nfaStates =
                function
                | []           -> Some nfaStates
                | states::rest ->
                    // states - all states in a single nfa that 
                    //          k-edge is currently in
                    match moveNFA kEdges states with
                    | [], _ -> None
                    | nfa   -> collectStates (nfa::nfaStates) rest

            collectStates [] kEdges.nfaStates
            |> Option.map (fun ns -> { kEdges with nfaStates = ns })

        // let currNode = snd mKEdges.lastEdge
        let sinkNode node = node, NULL_NODE, Map.empty
        let getOutEdges (u, v) = Graph.Nodes.outward v graph
                              |> Option.map (List.cons ^ sinkNode v)
                              |> Option.getOrElse []
        let outKEdges =
            mKEdges
            |> List.collect (fun kEdges -> 
                let pathIDs, edges = Map.toList kEdges.edges |> List.unzip
                let nextEdges =
                    List.map2 (fun path e -> 
                        getOutEdges e.lastEdge 
                        |> List.map (
                            fun x -> path, { e with lastEdge = basicEdge x })) 
                        pathIDs edges
                
                cartesian nextEdges
                |> List.map (fun es -> { kEdges with edges = Map.ofList es })
            )

        //  TODO: Use logary
        // printfn "outward edges for %d: %A" currNode
        //     (List.map (fun (u, v, _) -> u, v) outEdges)

        // Map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa.
        outKEdges |> List.choose moveKEdges

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

        let allNFAs  = Map.values nfaStates |> List.concat
        let allPaths = Map.keys nfaStates |> List.ofSeq
        let mKEdges  =
            // all k-nodes
            Graph.Nodes.toList graph
            |> konst |> List.init (Map.count nfaStates)
            |> cartesian
            // map them to MatchedKEdges
            |> List.map (fun es -> 
                { nfaStates = allNFAs
                  edges     = List.map2 (fun p (e, _) -> p, create p e) 
                                        allPaths es |> Map.ofList })

            // let createEdges (path, nfaStates) =
            //     List.map (fst >> MatchedEdge.create path nfaStates) allNodes

            // nfaStates |> Map.toList |> List.collect createEdges

        let checkMatched mKEdges =
            mKEdges.nfaStates
            |> List.forall (fst >> List.exists (fun t -> t.state = Matched))

        let rec bfs result mNodes =
            if List.isEmpty mNodes
            then result
            else
                let nextNodes          = nextKEdges graph mNodes
                let nodesMatched, rest = List.partition checkMatched nextNodes

                let mapInfo = List.map basicInfo
                // printfn "Nodes:\n %A"         ^ mapInfo mNodes
                // printfn "Matched nodes:\n %A" ^ mapInfo nodesMatched
                // printfn "Rest of nodes:\n %A" ^ mapInfo rest
                bfs (nodesMatched @ result) nextNodes

        nextKEdges graph mKEdges |> bfs []
        |> List.distinctBy (fun me -> Map.valueList me.edges 
                                      |> List.map basicInfo)