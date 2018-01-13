namespace OpraDB

open OpraDB.LangTypes

module NodeConstraints =

    let getOperator =
        function
        | Leq -> (<=)
        | Le  -> (<)
        | Geq -> (>=)
        | Ge  -> (>)
        | Eq  -> (=)
        | Neq -> (<>)

    let check edge graph (NodeConstraint (lhs, op, rhs)) ids =
        let op l r = getOperator op l r
        let valueOfLabelling name vars = IntLiteral 2

        let rec pred lhs rhs =
            match lhs, rhs with
            | IntLiteral    lhs, IntLiteral    rhs -> op lhs rhs
            | StringLiteral lhs, StringLiteral rhs -> op lhs rhs
            | Labelling (name, vars), rhs ->
                pred (valueOfLabelling name vars) rhs
            | lhs, Labelling (name, vars) ->
                pred lhs (valueOfLabelling name vars)
            | lhs, rhs ->
                printfn "WARNING: Mismatched types of operands %A and %A"
                    lhs rhs
                false

        pred lhs rhs