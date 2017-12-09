namespace OpraDB

open OpraDB.LangTypes
open FParsec
open FSharpx.Functional

module Parser = 

    /// Parse string
    let private str = pstring
    /// Parse whitespace 
    let private ws = spaces
    let private nameChar<'a> : Parser<_, 'a> = asciiLetter <|> digit
    let private name<'a>     : Parser<_, 'a> = many1Chars2 asciiLetter nameChar
    let private id<'a>       : Parser<_, 'a> = name .>> ws |>> ID

    /// example: s -[ pi ]-> t
    let private pathConstraint<'a> : Parser<_, 'a> =
              // source node
        pipe3 (ws >>. id)        
              // path name                                 
              (pstring "-[" >>. ws >>. id)   
              // target node                     
              (ws >>. pstring "]->" >>. ws >>. id .>> ws)
              PathConstraint.create 

    let private stringLiteral<'a> : Parser<_, 'a> =
        between (str "\"") (str "\"")
                (manySatisfy ((<>) '"'))
        <??> "string literal in double quotes"
        |>> StringLiteral

    let manyWith elem prefix = 
        pstring prefix >>. ws >>. pchar '(' >>. ws // prefix and opening bracket
        >>. (many elem)                            // TODO: what? list interpreter.fsof elements
        .>> (pchar ')' .>> ws)                     // closing bracket

    let private betweenChars first last = 
        between (pchar first) (pchar last)
 
    /// Parse @x or @'x where x is integer
    let private nodeVar<'a> : Parser<_, 'a> = 
        pchar '@' >>. pint32 |>> CurrNodeVar 
        <|> (pstring "@'" >>. pint32 |>> NextNodeVar)

    let private labelling<'a> : Parser<_, 'a> = 
        pipe2 (ws >>. id)
              (many nodeVar |> betweenChars '(' ')')
              (curry Labelling)

    let private operand<'a> : Parser<_, 'a> = 
        ws >>. choice [stringLiteral; pint32 |>> IntLiteral; labelling] .>> ws

    let private operator<'a> : Parser<_, 'a> = 
        let op s which = str s |>> konst which 
        
        ws >>. 
        choice [op "<=" Leq; op "<" Le; op ">" Ge  ; op ">=" Geq; op "=" Eq]
        .>> ws 

    /// examples: 
    /// [attr(@1) > 100] 
    /// [distance(@1 @'1) <= 10]
    let private nodeConstraint<'a> : Parser<_, 'a> = 
        pipe3 operand operator operand 
              (curry3 NodeConstraint)
        |> betweenChars '[' ']'

    let private regExp, regExpRef = 
        createParserForwardedToRef<RegularExpression, unit> ()

    /// example: [ E(@1) ](p q).* [attr(@1) > 100](p)
    do regExpRef :=
        choice [pchar '.' |>> konst AnyExp 
                nodeConstraint
                regExp .>> pchar '*' |>> StarExp 
                pipe2 regExp (ws >>. pchar '+' >>. regExp) (curry OrExp)
                pipe2 regExp (ws >>. regExp) (curry AndExp)]
      
    let rec regularConstraint =
        pipe2 !regExpRef
              (betweenChars '(' ')' (many id))
              (curry RegularConstraint)

    let private optionally ret p = p <|> (ws |>> konst ret)

    let parseQuery = 
        pipe3 (pstring "MATCH" >>. ws >>. (manyWith id "NODES")) // parse nodes
              (manyWith pathConstraint "SUCH THAT" |> optionally []) // path constraints
              (manyWith regularConstraint "WHERE" |> optionally [])  // path constraints
              (fun nodes pathConstrs regularConstrs -> 
                    { Query.empty with 
                        nodes              = nodes 
                        pathConstraints    = pathConstrs 
                        regularConstraints = regularConstrs })
        // .>> ws
        
