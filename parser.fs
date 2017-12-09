namespace OpraDB

open OpraDB.LangTypes
open FParsec
open FSharpx.Functional

module Parser = 
    let private nameChar<'a> : Parser<_, 'a> = asciiLetter <|> digit
    let private name<'a>     : Parser<_, 'a> = many1Chars2 asciiLetter nameChar
    let private id<'a>       : Parser<_, 'a> = name .>> spaces |>> ID

    /// example: s -[ pi ]-> t
    let pathConstraint<'a> : Parser<_, 'a> =
        pipe3 (spaces >>. id)                                         // source node
              (pstring "-[" >>. spaces >>. id)                        // path
              (spaces >>. pstring "]->" >>. spaces >>. id .>> spaces) // target node
              PathConstraint.create 

    /// Parse string
    let private str = pstring

    let stringLiteral<'a> : Parser<_, 'a> =
        between (str "\"") (str "\"")
                (manySatisfy ((<>) '"'))
        <??> "string literal in double quotes"
        |>> StringLiteral

    let manyWith elem prefix = 
        pstring prefix >>. spaces >>. pchar '(' >>. spaces // prefix and opening bracket
        >>. (many elem)                                    // list interpreter.fsof elements
        .>> (pchar ')' .>> spaces)                         // closing bracket

    let nodeVar<'a> : Parser<_, 'a> = 
        failwith "not implemented"

    let labelling<'a> : Parser<_, 'a> = 
        pipe2 (spaces >>. id)
              (between (pchar '(') (pchar ')')
                       (many nodeVar))
              (curry Labelling)

    let operand<'a> : Parser<_, 'a> = 
        spaces >>. 
        choice [stringLiteral; pint32 |>> IntLiteral; labelling] .>> spaces

    let operator<'a> : Parser<_, 'a> = 
        let op s which = str s |>> konst which 
        
        spaces >>. 
        choice [op "<=" Leq; op "<" Le; op ">" Ge  ; op ">=" Geq; op "=" Eq]
        .>> spaces 

    /// example: [ E(@1) ](p q).* [attr(@1) > 100](p)
    let regularConstraint<'a> : Parser<_, 'a> =
        spaces >>. pchar '[' 
        // pipe3 (spaces >>. id)                                         // source node
        //       (pstring "-[" >>. spaces >>. id)                        // path
        //       (spaces >>. pstring "]->" >>. spaces >>. id .>> spaces) // target node
        //       PathConstraint.create 

    let private optionally ret p = p <|> (spaces |>> konst ret)

    let parseQuery<'a> : Parser<_, 'a> = 
        pipe3 (pstring "MATCH" >>. spaces >>. (manyWith id "NODES")) // parse nodes
              (manyWith pathConstraint "SUCH THAT" |> optionally []) // path constraints
              (manyWith regularConstraint "WHERE" |> optionally [])  // path constraints
              (fun nodes pathConstrs regularConstrs -> 
                    { Query.empty with 
                        nodes              = nodes 
                        pathConstraints    = pathConstrs 
                        regularConstraints = regularConstrs })
        // .>> spaces
        
