namespace OpraDB

open OpraDB.AST
open OpraDB.ValueExpression

module NodeConstraints =

    exception WrongTypeException of ValueType 

    let checkKEdges letExps letQueriesRes kEdges graph nodeConstr =
        let labellingValue = Labelling.value letExps letQueriesRes kEdges graph

        match eval letQueriesRes kEdges labellingValue nodeConstr with 
        | Bool res -> res 
        | Null     -> false
        | other    -> raise (WrongTypeException (literalType other))
        