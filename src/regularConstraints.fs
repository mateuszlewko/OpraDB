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
                if NodeConstraints.checkKEdges kEdges graph constr ids
                then ok transition
                else []
            | Any   -> ok transition
            | Empty -> skipEmpty transition
                       |> List.collect (moveState kEdges ids)

        /// Move every state in a single NFA.
        let moveNFA edges (states, ids) =
            List.collect (moveState edges ids) states |> List.distinct, ids

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

        let sinkNode node      = node, NULL_NODE, Map.empty
        let getOutEdges (_, v) = Graph.Nodes.outward v graph
                                 |> Option.map (List.cons ^ sinkNode v)
                                 |> Option.getOrElse []
        let outKEdges =
            mKEdges
            |> List.collect (fun mkEdges -> 
                let pathIDs, edges = Map.toList mkEdges.currEdges |> List.unzip
                let nextEdges =
                    List.map2 (fun path e -> 
                        getOutEdges e.lastEdge 
                        |> List.map (
                            fun x -> path, { e with lastEdge = basicEdge x })) 
                        pathIDs edges
                
                List.cartesian nextEdges
                |> List.map (fun es -> { mkEdges with 
                                            currEdges = Map.ofList es })
            )

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
        let mKEdges  =
            let nodeFromIndex = 
                let rec getIndex ixMap (p : PathConstraint) = 
                    match Map.tryFind p.source ixMap with 
                    | None    -> Map.add p.source (Map.count ixMap) ixMap
                    | Some ix -> ixMap

                List.fold getIndex Map.empty query.pathConstraints

            // all k-nodes
            Graph.Nodes.toList graph |> List.map fst
            |> konst |> List.init (Map.count nodeFromIndex) |> List.cartesian
            // map them to MatchedKEdges
            |> List.map (fun es -> 
                let esArr  = Array.ofList es
                let kEdges = 
                    query.pathConstraints
                    |> List.map (fun (p : PathConstraint) -> 
                        let ix = Map.find p.source nodeFromIndex
                        p.path, create p.path esArr.[ix]
                    )                         

                { nfaStates = allNFAs
                  currEdges = kEdges |> Map.ofList }
            )

        let checkFinalNodes mKEdges =
            let endNodeNames = query.pathConstraints 
                               |> List.map (fun p -> p.path, p.target) 
                               |> Map.ofList

            let rec checkAll endNodeMappings = 
                function
                | []      -> true 
                | me::mes ->
                    let currEndNode = fst me.lastEdge
                    match Map.tryFind me.path endNodeNames with 
                    | None          -> checkAll endNodeMappings mes
                    | Some nodeName ->
                        match Map.tryFind nodeName endNodeMappings with 
                        | None -> Map.add nodeName currEndNode endNodeMappings 
                                  |> flip checkAll mes
                        | Some endNode -> if endNode <> currEndNode
                                          then false 
                                          else checkAll endNodeMappings mes

            checkAll Map.empty (Map.valueList mKEdges.currEdges)

        let checkNFAsInMatchedStates mKEdges =
            mKEdges.nfaStates
            |> List.forall (fst >> List.exists (fun t -> t.state = Matched))

        let checkMatched mKEdges = 
            checkNFAsInMatchedStates mKEdges && checkFinalNodes mKEdges
            
        let rec bfs visited result mNodes =
            if List.isEmpty mNodes
            then result
            else
                let nextNodes           = nextKEdges graph mNodes
                let nodesMatched, rest  = List.partition checkMatched nextNodes
                let nextNotVis          = nextNodes 
                                          |> List.filter (flip 
                                                Set.contains visited >> not) 

                // let mapMk = 
                //     List.map (fun mk -> 
                //         Map.toList mk.currEdges 
                //         |> List.map (fun (ID p, e) -> p, e.lastEdge)
                //         |> List.distinct
                //     )

                // printfn "Nodes:\n %A"         ^ mapMk mNodes
                // printfn "Matched nodes:\n %A" ^ mapMk nodesMatched
                // // printfn "Rest of nodes:\n %A" ^ mapMk rest
                // printfn "next of nodes not vis:\n %A" ^ mapMk nextNotVis
                // printfn "vis:\n %A" 
                //     (Set.toList visited 
                //      |> List.map (
                //         fun me -> (Map.valueList me.currEdges |> List.map info)
                //                   , List.map (fun (nf,_) -> List.map (fun t -> t.tid) nf) 
                //                             me.nfaStates)
                //     )
                
                let visited = nextNodes |> Set.ofList |> Set.union visited
                bfs visited (nodesMatched @ result) nextNotVis

        nextKEdges graph mKEdges |> bfs Set.empty []
        |> List.distinctBy (fun me -> Map.valueList me.currEdges 
                                      |> List.map basicInfo)