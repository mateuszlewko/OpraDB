namespace OpraDB 

module LangTypes = 

    type Identifier = ID of string 

    /// Path constraint is satisfied when there exists a path 
    /// from source to target
    type PathConstraint = {
            source : Identifier
            target : Identifier 
            path   : Identifier
        }

    /// NodeVariable x represents either @x or @'x, 
    /// for example CurrNodeVar 1 is @1 and NextNodeVar 2 is @'2
    type NodeVariable = CurrNodeVar of int | NextNodeVar of int

    /// Represents one of: <=, <, >=, >, =
    type Operator = Leq | Le | Geq | Ge | Eq

    type Operand = 
        /// LabellingFunction either checks whether there exists 
        /// label between specified nodes (or node if only one given), 
        /// or returns value of label, example: type(@1) = "bus"
        | Labelling of Identifier * NodeVariable list
        /// Represent int value in query
        | IntLiteral of int 
        /// String value (must be specified in quotes, example: "value")
        | StringLiteral of string
    
    type RegularExp   = Union of UnionRE | Simple of SimpleRE
     and UnionRE      = RegularExp * SimpleRE 
     and SimpleRE     = Concat of ConcatRE | Basic of BasicRE
     and ConcatRE     = SimpleRE * BasicRE 
     and BasicRE      = Star of ElementaryRE | Elementary of ElementaryRE
     and ElementaryRE = Group of GroupRE | Any 
     and GroupRE      = RegularExp


    type RegularExpression = 
        | Epsilon
        | AnyExp
        | NodeConstraint of Operand * Operator * Operand 
        | ConcatExp of RegularExpression * RegularExpression
        | UnionExp of RegularExpression * RegularExpression
        | StarExp of RegularExpression

    /// RegularExpression with paths applied to it,
    /// examples: .*[attr(@1) > 100](p)
    type RegularConstraint = 
        RegularConstraint of RegularExp * Identifier list

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