namespace OpraDB

open OpraDB.AST
open OpraDB.RegexNFA
open Hekate

module QueryData =

    /// All current states in a single NFA, along with
    /// id of paths applied to it
    type StatesInNFA = Transition list

    // TODO: Rename to TraversedEdge
    type MatchedEdge = {
            /// Path ID which this edge belongs to
            path         : Identifier
            /// First node on path
            source       : int Node
            /// Last visited edge
            lastEdge     : int Edge
            lastGoodEdge : int Edge

        }

    // TODO: Rename to EdgeVector
    type KEdges      = Map<Identifier, MatchedEdge>
    
    /// Map from summed ValueExprs to Literal option
    type ArithStates = Map<ValueExpr<unit>, Literal option>

    // TODO: This should be renamed to EdgeVector
    [<CustomEquality; CustomComparison>]
    type MatchedKEdges = {
            // TODO: rename to vector
            currEdges   : KEdges
            /// All states in every nfa
            nfas        : StatesInNFA list
            arithStates : ArithStates
        }
    with 
        override this.GetHashCode () = 
            let x = struct ( this.currEdges.GetHashCode ()
                           , this.nfas.GetHashCode () )
            x.GetHashCode ()

        override this.Equals other = 
            match other with 
            | :? MatchedKEdges as other -> 
                this.currEdges = other.currEdges && this.nfas = other.nfas 
            | _                -> false

        interface System.IComparable with 
            member this.CompareTo other = 
                compare (this.GetHashCode ()) (other.GetHashCode ())

    let [<Literal>] NULL_NODE = -1

    module MatchedEdge =
        let create path node = { source       = node
                                 path         = path
                                 lastEdge     = NULL_NODE, node
                                 lastGoodEdge = NULL_NODE, node }
                                         
        let basicInfo e = e.path, e.source, fst e.lastEdge
        let info e = e.path, e.source, e.lastEdge

    module MatchedKEdges =
        let basicInfo mkEs = Map.toList mkEs.currEdges |> List.map snd 