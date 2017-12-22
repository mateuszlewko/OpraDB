namespace OpraDB

module Graph = 

    type Edge = int * int 

    type PropertyValue = IntVal of int | StringVal of int 

    type Properties = Map<string, PropertyValue>

    type EdgeData = Map<Edge, Properties>

    type Node = {
            id         : int 
            data       : Properties
            neighbours : int Set            
        }
    
    type Graph = {
            nodes    : Map<int, Node>
            edgeData : EdgeData
        }