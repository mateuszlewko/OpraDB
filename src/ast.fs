namespace OpraDB

module AST =

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
    // type NodeVariable = CurrNodeVar of int | NextNodeVar of int
    type NodeVariable = CurrNodeVar of Identifier | NextNodeVar of Identifier
   
    module NodeVariable = 
        let identifier = function CurrNodeVar i -> i | NextNodeVar i -> i
   
    /// Represents one of: <=, <, >=, >, =, <>
    type Operator = Leq | Le | Geq | Ge | Eq | Neq

    type Literal = IntLit of int | StringLit of string

    type Operand =
        /// LabellingFunction either checks whether there exists
        /// label between specified nodes (or node if only one given),
        /// or returns value of label, example: type(@1) = "bus"
        | Labelling of Identifier * NodeVariable list
        /// Represent int value in query
        | IntLiteral of int
        /// String value (must be specified in quotes, example: "value")
        | StringLiteral of string
        // | NodeVariable TODO: Handle this case

    module Operand = 
        open NodeVariable

        let allPathIDs = 
            function 
            | Labelling (_, vars) -> List.map identifier vars |> List.distinct
            | _                   -> []

    type NodeConstraint = NodeConstraint of Operand * Operator * Operand
    
    module NodeConstraint = 
        open Operand

        let allPathIDs (NodeConstraint (l, _, r)) = allPathIDs l @ allPathIDs r 
                                                    |> List.distinct


    type RegularExpression =
        | AnyExp
        | NodeExp of NodeConstraint
        | ConcatExp of RegularExpression * RegularExpression
        | UnionExp of RegularExpression * RegularExpression
        | StarExp of RegularExpression

    module RegularExpression = 
        open NodeConstraint

        let allPathIDs = 
            let rec get curr = 
                function 
                | NodeExp constr -> curr @ allPathIDs constr |> List.distinct
                | UnionExp (r1, r2) | ConcatExp (r1, r2) ->
                    get (get curr r1) r2
                | _ -> []

            get []

    module PathConstraint =
        let create source path target = {
                source = source
                path   = path
                target = target
            }

    type ArithOperand = 
        /// SumBy (Path Identifier, Label Identifier)
        | SumBy of Identifier * Identifier 
        | IntALiteral of int

    type ArithmeticConstraint = 
        ArithmeticConstraint of ArithOperand * Operator * ArithOperand

    type Query = {
            /// Matched nodes
            nodes                 : Identifier list
            /// Matched paths
            paths                 : Identifier list
            pathConstraints       : PathConstraint list
            regularConstraints    : RegularExpression list
            arithmeticConstraints : ArithmeticConstraint list
        }

    module Query =
        let empty = { nodes                 = []
                      paths                 = []
                      pathConstraints       = []
                      regularConstraints    = []
                      arithmeticConstraints = [] }