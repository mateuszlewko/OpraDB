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
    let nodeConstraint<'a> : Parser<_, 'a> = 
        // TODO: Add node constrain which returns bool (only one operand in expression)
        pipe3 operand operator operand 
              (curry3 NodeConstraint)
        |> betweenChars '[' ']'

    // TODO: Remove this (only for debugging)
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> = 
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    module RegexParser = 

        // TODO: make private
        type RegExpTail = 
            | Concat of RegExpTail * RegExpAux
            | Any of RegExpAux
            | NodeCon of NodeConstraint * RegExpAux

        and RegExpAux = 
            | Star of RegExpAux 
            | Union of RegExpTail * RegExpAux 
            | ConcatAux of RegExpTail * RegExpAux
            | Epsilon

        let private regExp, regExpRef = 
            createParserForwardedToRef<RegExpTail, unit> ()

        let private regExpAux, regExpAuxRef = 
            createParserForwardedToRef<RegExpAux, unit> ()

        do regExpRef :=
            choice [betweenChars '(' ')' regExp .>>. regExpAux |>> Concat
                    pchar '.' >>. regExpAux |>> Any
                    nodeConstraint .>>. regExpAux |>> NodeCon]
        do regExpAuxRef := 
            choice [pchar '*' >>. regExpAux |>> Star 
                    pchar '+' >>. regExp .>>. regExpAux |>> Union
                    regExp .>>. regExpAux |>> ConcatAux 
                    ws |>> konst Epsilon] 
    
        let regExpParser = regExp
        let regularExpression = 
            regExp |>> (fun re -> RegularExpression.Epsilon)
      
    let rec regularConstraint : Parser<RegularConstraint, unit> =
        pipe2 RegexParser.regularExpression
              (betweenChars '(' ')' (many id))
              (curry RegularConstraint)

    let private optionally ret p = p <|> (ws |>> konst ret)

    let parseQuery : Parser<Query, unit> = 
        pipe3 (pstring "MATCH" >>. ws >>. (manyWith id "NODES")) // parse nodes
              (manyWith pathConstraint "SUCH THAT" |> optionally []) // path constraints
              (manyWith regularConstraint "WHERE" |> optionally []) // regular constraints
              (fun nodes pathConstrs regularConstrs -> 
                    { Query.empty with 
                        nodes              = nodes 
                        pathConstraints    = pathConstrs 
                        regularConstraints = regularConstrs })
        .>> ws
        
