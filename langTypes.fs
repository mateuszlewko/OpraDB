namespace OpraDB 

module LangTypes = 
    type Identifier = ID of string 

    type Nodes = Identifier list

    type Paths = Identifier list

    type PathConstraint = {
            source : Identifier
            target : Identifier 
            path   : Identifier
        }

    module PathConstraint = 
        let create source path target = {
                source = source 
                path   = path 
                target = target 
            }

    type Query = {
            nodes           : Nodes 
            paths           : Paths 
            pathConstraints : PathConstraint list
        }

    module Query = 
        let empty = { nodes = []; paths = []; pathConstraints = [] }