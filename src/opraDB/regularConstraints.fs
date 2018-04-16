namespace OpraDB

open FSharpx.Functional

open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.QueryData
open OpraDB.QueryData.MatchedEdge
open OpraDB.Data
open OpraDB.CommonUtils
open OpraDB.ArithmeticConstraints

open FSharpx.Collections
open FSharpx
open Hekate

module RegularConstraints =
          
    let private basicEdge (u, v, _) = u, v

    /// Get all outward edges of a given node, which have at least one
    /// state in every NFA they belong to.
    let private nextKEdges graph (mKEdges : MatchedKEdges list) preds =
        /// Move to states reachable within one transition in single NFA
        /// for a given kEdges. Skips empty states.
        let rec makeTransition kEdges transition =
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
                if NodeConstraints.checkKEdges kEdges graph constr
                then ok transition
                else []
            | Any   -> ok transition
            | Empty -> skipEmpty transition
                       |> List.collect (makeTransition kEdges)

        /// Move every state in a single NFA.
        let moveNFA edges states =
            List.collect (makeTransition edges) states |> List.distinct

        /// Try to move every state in all NFAs for a given k-edge.
        let moveNFAStates mKEdges =
            let rec collectStates nfaStates =
                function
                | []           -> Some nfaStates
                | states::rest ->
                    // states - all states in a single nfa that 
                    //          k-edge is currently in
                    match moveNFA mKEdges.currEdges states with
                    | []  -> None
                    | nfa -> collectStates (nfa::nfaStates) rest

            collectStates [] mKEdges.nfas
            |> Option.map (fun ns -> { mKEdges with nfas = ns })

        let sinkNode node      = node, NULL_NODE, Map.empty
        let getOutEdges (_, v) = Graph.Nodes.outward v graph
                                 |> Option.map (List.cons ^ sinkNode v)
                                 |> Option.getOrElse [sinkNode v]
                                //  |> Option.getOrElse [] // This version changes
                                                           // behaviour
        let outKEdges =
            mKEdges // TODO: mKEdges and mkEdges?? Bad!
            |> List.collect (fun mkEdges -> 
                let pathIDs, edges = Map.toList mkEdges.currEdges |> List.unzip
                let nextEdges =
                    List.map2 (fun path e -> 
                        getOutEdges e.lastEdge 
                        |> List.map (
                            fun x -> path, { e with lastEdge = basicEdge x })) 
                        pathIDs edges
                
                List.cartesian nextEdges
                |> List.map (fun es -> mkEdges
                                      , { mkEdges with 
                                            currEdges = Map.ofList es })
            )

        // Map neighbouring edges to MatchedEdges.
        // MatchedEdge is an edge that has at least one state in every nfa.
        let preds, es = 
            let rec moveStatesAndPreds preds es = 
                function
                | []         -> preds, es 
                | (pred, curr)::rest -> 
                    match moveNFAStates curr with 
                    | Some next -> let preds = MultiMap.add next pred preds
                                   moveStatesAndPreds preds (next::es) rest
                    | None      -> moveStatesAndPreds preds es rest 
           
            moveStatesAndPreds preds [] outKEdges
        // outKEdges 
        // |> List.choose (fun (pred, curr) -> moveNFAStates curr 
                                           // |> Option.map (tuple2 pred))
        preds, List.map (updateArithStates graph) es

    /// Get nodes that match regular constraints in a given query.
    let matchEdges (graph : Graph) (query : BasicQuery) =
        let allNFAs = query.regularConstraints 
                      |> List.map (fun e -> [State.ofRegExp e]) 
        let mKEdges =
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

                { nfas        = allNFAs
                  currEdges   = kEdges |> Map.ofList
                  arithStates = createArithStates query 
                }
                |> updateArithStates graph
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
            mKEdges.nfas
            |> List.forall (List.exists (fun t -> t.state = Matched))

        let checkMatched preds mKEdges =
            checkNFAsInMatchedStates mKEdges 
            && checkFinalNodes mKEdges
            && inequalitiesSatisfied mKEdges preds graph 
                                     query.arithmeticConstraints
            
        let mapMk = 
            List.map (fun mk -> 
                Map.toList mk.currEdges 
                |> List.map (fun (ID p, e) -> p, e.lastEdge)
                |> List.distinct
            )

        let rec bfs visited result preds mNodes =
            if List.isEmpty mNodes
            then
                // printfn "preds:" 
                // Map.toList preds |> List.iter (fun (k, v) -> 
                //                         printfn "%A" (MatchedKEdges.basicInfo k)
                //                         printfn "%A" (mapMk (Set.toList v))
                //                     )
                                    
                result
            else
                let preds, nextNodes    = nextKEdges graph mNodes preds
                let nodesMatched, rest  = List.partition (checkMatched preds) 
                                                         nextNodes
                let nextNotVis          = nextNodes 
                                          |> List.filter (flip 
                                                Set.contains visited >> not) 

                // printfn "Nodes:\n %A"         ^ mapMk mNodes
                // printfn "Matched nodes:\n %A" ^ mapMk nodesMatched
                // // printfn "Rest of nodes:\n %A" ^ mapMk rest
                // printfn "next of nodes not vis:\n %A" ^ mapMk nextNotVis
 
                // Set.toList visited 
                // |> List.map (
                //     fun me -> (Map.valueList me.currEdges |> List.map info)
                //              , List.map (List.map (fun t -> t.tid)) me.nfas)
                
                // |> printfn "vis:\n %A"
                
                let visited = nextNodes |> Set.ofList |> Set.union visited
                bfs visited (nodesMatched @ result) preds nextNotVis

        nextKEdges graph mKEdges Map.empty 
        |> uncurry (bfs Set.empty []) 
        |> List.distinctBy (fun me -> Map.valueList me.currEdges 
                                      |> List.map basicInfo)