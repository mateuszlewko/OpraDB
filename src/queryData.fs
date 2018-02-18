namespace OpraDB

open OpraDB.AST
open OpraDB.RegexNFA
open Hekate

module QueryData =

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
            currEdges : KEdges
            nfaStates : NFAState list
        }

    let [<Literal>] NULL_NODE = -1

    module MatchedEdge =
        let create path node = { source    = node
                                 path      = path
                                 lastEdge  = NULL_NODE, node }
                                         
        let basicInfo e = e.path, e.source, fst e.lastEdge