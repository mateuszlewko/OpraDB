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
   
    /// Represents one of: <=, <, >=, >, =, <>, and, or, is, isNot
    type BoolOperator  = Leq | Le | Geq | Ge | Eq | Neq | And | Or | Is | IsNot
    
    type ArithOperator = Add | Sub | Mult | Div 

    type Literal = 
        | Int    of int 
        | Bool   of bool 
        | Float  of float
        | String of string
        | Null

    type ValueExpr<'ext> = 
        | Lit of Literal
        | Labelling of Identifier * NodeVariable list
        | ArithOp of ValueExpr<'ext> * ArithOperator * ValueExpr<'ext>
        | BoolOp of ValueExpr<'ext> * BoolOperator * ValueExpr<'ext>
        | Ext of 'ext

    type NodeConstraint = ValueExpr<unit>

    type RegularExpression =
        | AnyExp
        | NodeExp   of NodeConstraint
        | ConcatExp of RegularExpression * RegularExpression
        | UnionExp  of RegularExpression * RegularExpression
        | StarExp   of RegularExpression
        | LetCall   of Identifier * NodeVariable list

    type RE = RegularExpression

    module PathConstraint =
        let create source path target = {
                source = source
                path   = path
                target = target
            }

    type ArithmeticConstraint = 
        | Sum     of ValueExpr<unit>
        | Value   of ValueExpr<ArithmeticConstraint>

    type AC = ArithmeticConstraint

    type BasicQuery = {
            /// Matched nodes
            nodes                 : Identifier list
            /// Matched paths
            paths                 : Identifier list
            /// 'SUCH THAT ...' constraints
            pathConstraints       : PathConstraint list
            regularConstraints    : RegularExpression list
            arithmeticConstraints : ArithmeticConstraint list
        }

    type LetBody =
        | Query   of BasicQuery 
        | Regular of RegularExpression
        | Value   of ValueExpr<unit>
        | Arith   of ArithmeticConstraint

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