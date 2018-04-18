namespace OpraDB

open Hekate

module Data =     

    type LabelValue = Lit of AST.Literal | Str of string

    type Labels = Map<string, LabelValue>

    type Graph = MGraph<int, Labels, Labels>

    let buildGraph nodes edges : Graph =
        Graph.create nodes edges