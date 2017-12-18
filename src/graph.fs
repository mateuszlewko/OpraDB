namespace OpraDB

module Graph = 

    type Edge = int * int 
    type PropertyValue = IntVal of int | StringVal of int 
    type Properties = Map<string, PropertyValue>
    type EdgeData = Map<Edge, Properties>

    type NodeMap = Map<int, Node> 
    and Node = {
            id         : int 
            data       : Properties
            neighbours : NodeMap
        }
    