namespace OpraDB

open Hekate

module Data =     

    type Labels = Map<string, AST.Literal>

    type Graph = MGraph<int, Labels, Labels>

    let buildGraph nodes edges : Graph =
        Graph.create nodes edges