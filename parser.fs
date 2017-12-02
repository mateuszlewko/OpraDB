namespace OpraDB

open OpraDB.LangTypes
open FParsec

// open FParsec.Primitives
// open FParsec.CharParsers

module Parser = 
    let nameChar<'a> : Parser<_, 'a> = asciiLetter <|> digit
    let name<'a>     : Parser<_, 'a> = many1Chars2 asciiLetter nameChar
    let id<'a>       : Parser<_, 'a> = name .>> spaces |>> ID

    // s -[ pi ]-> t
    let pathConstraint<'a> : Parser<_, 'a> =
        pipe3 (spaces >>. id) // source node
              (pstring "-[" >>. spaces >>. id) // path
              (spaces >>. pstring "]->" >>. spaces >>. id .>> spaces) // target node
              PathConstraint.create

    let manyWith elem prefix = 
        pstring prefix >>. spaces >>. pchar '(' >>. spaces >>. (many elem) 
        .>> (pchar ')' .>> spaces)

    let parseQuery<'a> : Parser<_, 'a> = 
        pipe2 (pstring "MATCH" >>. spaces >>. (manyWith id "NODES")) 
              (spaces >>. manyWith pathConstraint "SUCH THAT") 
              (fun nodes pathConstrs -> { nodes = nodes 
                                          paths = []
                                          pathConstraints = pathConstrs })
        .>> spaces

