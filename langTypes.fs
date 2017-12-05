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

    type FreeVariable = Var of Identifier
    
    type Operator = Leq | Le | Geq | Ge | Eq
    
    type NodeExpression = 
        NodeExpression of FreeVariable * Operator * FreeVariable
        
    type Lambda = Lambda of Identifier

    type LambdaExpression = 
        LambdaExp of Lambda * FreeVariable list
    
    type NodeConstraint = LambdaExpression | NodeExpression

    type RegularConstraint = 
        | NodeConstraint 
        | AndConstraint of RegularConstraint * RegularConstraint
        | OrConstraint of RegularConstraint * RegularConstraint
        | StarConstraint of RegularConstraint

    module PathConstraint = 
        let create source path target = {
                source = source 
                path   = path 
                target = target 
            }

    type Query = {
            nodes              : Nodes 
            paths              : Paths 
            pathConstraints    : PathConstraint list
            regularConstraints : RegularConstraint list
        }

    module Query = 
        let empty = { nodes              = []
                      paths              = []
                      pathConstraints    = []
                      regularConstraints = [] }