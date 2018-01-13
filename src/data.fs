namespace OpraDB

open Hekate

module Data =

    // type Edge = int * int

    type LabelValue = IntVal of int | StringVal of int

    type Labels = Map<string, LabelValue>

    // type EdgeData = Map<Edge, Properties>

    type Graph = MGraph<int, Labels, Labels>

    // type Node = {
    //         id         : int
    //         data       : Properties
    //         neighbours : int Set
    //     }

    // type Graph = {
    //         nodes    : Map<int, Node>
    //         edgeData : EdgeData
    //     }