namespace OpraDB

open OpraDB.LangTypes
open FParsec
open FSharpx.Functional

module Parser = 
    let nameChar<'a> : Parser<_, 'a> = asciiLetter <|> digit
    let name<'a>     : Parser<_, 'a> = many1Chars2 asciiLetter nameChar
    let id<'a>       : Parser<_, 'a> = name .>> spaces |>> ID

    // s -[ pi ]-> t
    let pathConstraint<'a> : Parser<_, 'a> =
        pipe3 (spaces >>. id)                                         // source node
              (pstring "-[" >>. spaces >>. id)                        // path
              (spaces >>. pstring "]->" >>. spaces >>. id .>> spaces) // target node
              PathConstraint.create 

// route(p) <T>* <attr(@1) > 100>(p)
    let private str = pstring

    let operator<'a> : Parser<_, 'a> = 
        let op s which = str s |>> konst which 
        
        spaces >>. 
        choice [op "<=" Leq; op "<" Le; op ">" Ge  ; op ">=" Geq; op "=" Eq]
        .>> spaces 

    let stringLiteral<'a> : Parser<_, 'a> =
        between (str "\"") (str "\"")
                (manySatisfy ((<>) '"'))
        <??> "string literal in double quotes"
        |>> StringLiteral

    let labelling = () // TODO: implement

    let operand<'a> : Parser<_, 'a> = 
        spaces >>. choice [stringLiteral; pint32 |>> IntLiteral] .>> spaces

    let regularConstraint<'a> : Parser<_, 'a> =
        spaces >>. pchar '[' 
        // pipe3 (spaces >>. id)                                         // source node
        //       (pstring "-[" >>. spaces >>. id)                        // path
        //       (spaces >>. pstring "]->" >>. spaces >>. id .>> spaces) // target node
        //       PathConstraint.create 

    let manyWith elem prefix = 
        pstring prefix >>. spaces >>. pchar '(' >>. spaces // prefix and opening bracket
        >>. (many elem)                                    // list interpreter.fsof elements
        .>> (pchar ')' .>> spaces)                         // closing bracket

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
        
