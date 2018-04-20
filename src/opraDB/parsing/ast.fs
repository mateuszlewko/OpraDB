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
    /// for example CurrNodeVar p is @p and NextNodeVar p is @'p
    type NodeVariable = CurrNodeVar of Identifier | NextNodeVar of Identifier
   
    module NodeVariable = 
        let identifier = function CurrNodeVar i | NextNodeVar i -> i
   
    /// Represents one of: <=, <, >=, >, =, <>, and, or
    type BoolOperator  = Leq | Le | Geq | Ge | Eq | Neq | And | Or | Is | IsNot
    type ArithOperator = Add | Sub | Mult | Div 

    // type Literal = IntLit of int | StringLit of string

    type Literal = 
        | Int of int 
        | Bool of bool 
        | Float of float
        | String of string
        | Null

    type ValueExpr<'ext> = 
        | Lit of Literal
        | Labelling of Identifier * NodeVariable list
        | ArithOp of ValueExpr<'ext> * ArithOperator * ValueExpr<'ext>
        | BoolOp of ValueExpr<'ext> * BoolOperator * ValueExpr<'ext>
        | Ext of 'ext

    // type Operand =
    //     /// LabellingFunction either checks whether there exists
    //     /// label between specified nodes (or node if only one given),
    //     /// or returns value of label, example: type(@1) = "bus"
    //     /// Represent int value in query
    //     | IntLiteral of int
    //     /// String value (must be specified in quotes, example: "value")
    //     | StringLiteral of string
    //     // | NodeVariable TODO: Handle this case

    // module ValueExpr = 
    //     open NodeVariable
         
    //     let allPathIDs = 
    //         function 
    //         | Labelling (_, vars) -> List.map identifier vars |> List.distinct
    //         | _                   -> []

    type NodeConstraint = ValueExpr<unit>
        // | Labelling of Identifier * NodeVariable list
        // | Value of ValueExpr<NodeConstraint>
    
    module NodeConstraint = 
        open NodeVariable

        let allPathIDs = 
            function 
            | Labelling (_, vars) -> List.map identifier vars |> List.distinct
            | _                   -> []

        // let allPathIDs  = 
            
        //     allPathIDs l @ allPathIDs r 
        //                                             |> List.distinct


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
                | NodeExp constr     -> curr @ allPathIDs constr 
                                        |> List.distinct
                | UnionExp (r1, r2)
                | ConcatExp (r1, r2) -> get (get curr r1) r2
                | _                  -> []

            get []

    module PathConstraint =
        let create source path target = {
                source = source
                path   = path
                target = target
            }

    // type ArithOperand =
    //     /// SumBy (Path Identifier, Label Identifier)
    //     | SumBy of Identifier * Identifier 
    //     | IntALiteral of int
    //     | Add of ArithOperand * ArithOperand
    //     | Mult of ArithOperand * ArithOperand
   
    type ArithmeticConstraint = 
        | Sum of ValueExpr<unit>
        | Value of ValueExpr<ArithmeticConstraint>

    // type AC = ArithmeticConstraint
        // ArithmeticConstraint of ArithOperand * Operator * ArithOperand

    type BasicQuery = {
            /// Matched nodes
            nodes                 : Identifier list
            /// Matched paths
            paths                 : Identifier list
            pathConstraints       : PathConstraint list
            regularConstraints    : RegularExpression list
            arithmeticConstraints : ArithmeticConstraint list
        }

    type LetBody =
        | Query of BasicQuery 
        | Regular of RegularExpression
        | Node of NodeConstraint
        | Arith of ArithmeticConstraint
        // | Value of 

    type LetExp = {  
            name : Identifier
            args : Identifier list 
            body : LetBody  
        }
    
    type Query = {
            letExps : LetExp list
            basic   : BasicQuery
        }

    module Query = 
        let empty = { letExps = []
                      basic   = { nodes                 = []
                                  paths                 = []
                                  pathConstraints       = []
                                  regularConstraints    = []
                                  arithmeticConstraints = [] }
                    }