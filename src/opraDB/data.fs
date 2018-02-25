namespace OpraDB

open Hekate

module Data =

    type LabelValue = IntVal of int | StringVal of string

    type Labels = Map<string, LabelValue>

    type Graph = MGraph<int, Labels, Labels>

    let buildGraph nodes edges =
        Graph.empty