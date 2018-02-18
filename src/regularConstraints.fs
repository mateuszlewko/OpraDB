namespace OpraDB

open FSharpx.Functional

open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.QueryData
open OpraDB.QueryData.MatchedEdge
open OpraDB.Data
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =
          
    let private basicEdge (u, v, _) = u, v

    /// Get all outward edges of a given node, which have at least one
    /// state in every NFA they belong to.
    let private nextKEdges graph (mKEdges : MatchedKEdges list) =
        /// Move to states reachable within one transition in single NFA
        /// for a given kEdges. Skips empty states.
        let rec moveState (kEdges : KEdges) ids transition =
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
                if NodeConstraints.checkKEdges kEdges graph constr ids
                then ok transition
                else []
            | Any   -> ok transition
            | Empty -> skipEmpty transition
                       |> List.collect (moveState kEdges ids)

        /// Move every state in a single NFA.
        let moveNFA edges (states, ids) =
            List.collect (moveState edges ids) states, ids

        /// Try to move every state in all NFAs for a given k-edge.
        let moveKEdges mKEdges =
            let rec collectStates nfaStates =
                function
                | []           -> Some nfaStates
                | states::rest ->
                    // states - all states in a single nfa that 
                    //          k-edge is currently in
                    match moveNFA mKEdges.currEdges states with
                    | [], _ -> None
                    | nfa   -> collectStates (nfa::nfaStates) rest

            collectStates [] mKEdges.nfaStates
            |> Option.map (fun ns -> { mKEdges with nfaStates = ns })

        // let currNode = snd mKEdges.lastEdge
        let sinkNode node = node, NULL_NODE, Map.empty
        let getOutEdges (u, v) = Graph.Nodes.outward v graph
                              |> Option.map (List.cons ^ sinkNode v)
                              |> Option.getOrElse []
        let outKEdges =
            mKEdges
            |> List.collect (fun kEdges -> 
                let pathIDs, edges = Map.toList kEdges.currEdges |> List.unzip
                let nextEdges =
                    List.map2 (fun path e -> 
                        getOutEdges e.lastEdge 
                        |> List.map (
                            fun x -> path, { e with lastEdge = basicEdge x })) 
                        pathIDs edges
                
                List.cartesian nextEdges
                |> List.map (fun es -> { kEdges with currEdges = Map.ofList es })
            )

        //  TODO: Use logary
        let mapMk = List.map (fun mk -> mk.currEdges)
        // printfn "outward edges for %A, are: %A" (mapMk mKEdges)
        //     (mapMk outKEdges)

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
            let nl _ = printfn ""
            // all k-nodes
            Graph.Nodes.toList graph |> List.map fst
            |> konst |> List.init (Map.count nfaStates) |> List.cartesian
            // |>! List.iter (List.iter (printf "%d ") >> nl)
            // map them to MatchedKEdges
            |> List.map (fun es -> 
                // List.iter (printf "%d ") es 
                // printfn ""

                let kEdges = List.map2 (fun p e -> p, create p e) allPaths es 
                { nfaStates = allNFAs
                  currEdges = kEdges |> Map.ofList }
            )

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
                let mapMk = List.map (fun mk -> mk.currEdges)

                let mapInfo = List.map basicInfo
                // printfn "Nodes:\n %A"         ^ mapMk mNodes
                // printfn "Matched nodes:\n %A" ^ mapMk nodesMatched
                // printfn "Rest of nodes:\n %A" ^ mapMk rest
                bfs (nodesMatched @ result) nextNodes

        nextKEdges graph mKEdges |> bfs []
        |> List.distinctBy (fun me -> Map.valueList me.currEdges 
                                      |> List.map basicInfo)