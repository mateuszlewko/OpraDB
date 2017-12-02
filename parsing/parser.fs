namespace OpraDB

open FParsec
// open FParsec.Primitives
// open FParsec.CharParsers

module Lang = 
    type Identifier = ID of string 

    type Nodes = Identifier list

    type Paths = Identifier list

    type PathConstraint = {
            source : Identifier
            target : Identifier 
            path   : Identifier
        }

    module PathConstraint = 
        let create source path target = {
                source = source 
                path   = path 
                target = target 
            }

    type Query = {
            nodes           : Nodes 
            paths           : Paths 
            pathConstraints : PathConstraint list
        }

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
        pstring prefix >>. spaces >>. (many elem) .>> spaces

    

