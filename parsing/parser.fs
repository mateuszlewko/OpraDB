namespace OpraDB

open FParsec
open FParsec.Primitives
open FParsec.CharParsers
// open Lang

module Lang = 
    type Identifier = ID of string 

    type Nodes = Identifier list

    type Paths = Identifier list

    type PathConstraint = Constraint of (int -> int -> bool)

    type Query = {
            nodes           : Nodes 
            paths           : Paths 
            pathConstraints : PathConstraint list
        }

    let nameChar<'a> : Parser<_, 'a> = asciiLetter <|> digit
    let name<'a> : Parser<_, 'a> = many1Chars2 asciiLetter nameChar
    let id<'a> : Parser<_, 'a> = name .>> spaces |>> ID

    let manyIDWith prefix = 
        pstring prefix >>. spaces >>. (many id) .>> spaces

