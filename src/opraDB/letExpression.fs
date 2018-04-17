namespace OpraDB 

open OpraDB.AST

module LetExpression = 

    type LetTypeKind = ArithKind | NodeKind | RegularKind | QueryKind 
    exception InvalidLetTypeException of LetTypeKind * LetTypeKind
    exception InvalidLetUsageException

    let private operand lets = 
        function 
        | Labelling (label, vars) as l -> 
            match Map.tryFind label lets with 
            | None     -> l 
            | Some exp -> raise InvalidLetUsageException
                // match exp.body with 
                // | NodeConstr constr -> constr
                // | other             ->  
        | other -> other 
        
    // let private arith lets = 

    let rec regExp lets =
        function
        | AnyExp -> AnyExp
        | NodeExp constr -> AnyExp

    let inlineLetExprs letExps query = 
        let lets = List.map (fun l -> l.name, l) letExps |> Map.ofList
        ()