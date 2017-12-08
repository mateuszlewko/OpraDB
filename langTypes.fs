namespace OpraDB 

module LangTypes = 

    type Identifier = ID of string 

    // type Nodes = Identifier list

    // /// Matched paths
    // type Paths = Identifier list

    /// Path constraint is satisfied when there exists a path 
    /// from source to target
    type PathConstraint = {
            source : Identifier
            target : Identifier 
            path   : Identifier
        }

    type FreeVariable = FreeVar of Identifier

    /// NodeVariable x represents either @x or @'x, 
    /// for example CurrNodeVar 1 is @1 and NextNodeVar 2 is @'2
    type NodeVariable = CurrNodeVar of int | NextNodeVar of int

    /// LabellingFunction either checks whether there exists 
    /// label between specified nodes (or node if only one given), 
    /// or returns value of <para/> label 
    // type LabellingFunction = 
    //     Labelling of Identifier * FreeVariable list
    // type LabellingFunction = 
    //     Labelling of Identifier * FreeVariable list
    
    /// Represent int value in query
    type IntValue = IntVal of int

    /// String value (must be specified in quotes, example: "value")
    type StringValue = StringVal of string

    // type ExpressedValue = IntValue | FreeVariable

    /// Represents one of: <=, <, >=, >, =
    type Operator = Leq | Le | Geq | Ge | Eq

    type Operand = 
        | Labelling of Identifier * NodeVariable list
        | IntVal of int 
        | StringVal of string
    
    type NodeConstraint = NodeConstraint of Operand * Operator * Operand
    
    type RegularConstraint = 
        | Any
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
            /// Matched nodes
            nodes              : Identifier list
            /// Matched paths
            paths              : Identifier list 
            pathConstraints    : PathConstraint list
            regularConstraints : RegularConstraint list
        }

    module Query = 
        let empty = { nodes              = []
                      paths              = []
                      pathConstraints    = []
                      regularConstraints = [] }