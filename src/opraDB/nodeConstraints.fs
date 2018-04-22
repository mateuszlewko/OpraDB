namespace OpraDB

open OpraDB.AST
open OpraDB.ValueExpression

module NodeConstraints =

    exception WrongTypeException of ValueType 

    let checkKEdges letExps kEdges graph nodeConstr =
        let labellingValue = Labelling.value letExps kEdges graph

        match eval labellingValue nodeConstr with 
        | Bool res -> res 
        | Null     -> false
        | other    -> raise (WrongTypeException (literalType other))
        