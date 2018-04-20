namespace OpraDB

open OpraDB.AST
open OpraDB.RegexNFA
open Hekate

module QueryData =

    /// All current states in a single NFA, along with
    /// id of paths applied to it
    type StatesInNFA = Transition list

    type MatchedEdge = {
            path      : Identifier
            source    : int Node
            lastEdge  : int Edge
        }

    type KEdges      = Map<Identifier, MatchedEdge>
    type ArithStates = Map<Identifier * Identifier, Literal>

    // TODO: This should be renamed to EdgesVector
    [<CustomEquality; CustomComparison>]
    type MatchedKEdges = {
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
                this.currEdges = other.currEdges 
             && this.nfas      = other.nfas 
            | _                -> false

        interface System.IComparable with 
            member this.CompareTo other = 
                compare (this.GetHashCode()) (other.GetHashCode())

    let [<Literal>] NULL_NODE = -1

    module MatchedEdge =
        let create path node = { source    = node
                                 path      = path
                                 lastEdge  = NULL_NODE, node }
                                         
        let basicInfo e = e.path, e.source, fst e.lastEdge
        let info e = e.path, e.source, e.lastEdge

    module MatchedKEdges =
        let basicInfo mkEs = Map.toList mkEs.currEdges |> List.map snd 