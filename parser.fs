namespace OpraDB

open OpraDB.LangTypes
open FParsec
open FSharpx.Functional

#nowarn "40"

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
        pipe3 operand operator operand 
              (curry3 NodeConstraint)
        |> betweenChars '[' ']'

    // let private regExp, regExpRef = 
    //     createParserForwardedToRef<RegularExp, unit> ()

    /// example: [ E(@1) ](p q).* [attr(@1) > 100]
    // let orExp = 
    //     ws >>. pchar '+' >>. regExp

    // let regularExpression =
    // and basicRE = 

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    // TODO: make private
    module RegexParser = 
        type RegExp = 
            | Concat of RegExp * RegExpAux
            | Any of RegExpAux
        and RegExpAux = 
            | Star of RegExpAux 
            | Union of RegExp * RegExpAux 
            | ConcatAux of RegExp * RegExpAux
            | Epsilon

        let private regExp, regExpRef = 
            createParserForwardedToRef<RegExp, unit> ()

        let private regExpAux, regExpAuxRef = 
            createParserForwardedToRef<RegExpAux, unit> ()

        do regExpRef :=
            choice [betweenChars '(' ')' regExp .>>. regExpAux |>> Concat
                    pchar '.' >>. regExpAux |>> Any]
        do regExpAuxRef := 
            choice [pchar '*' >>. regExpAux |>> Star 
                    pchar '+' >>. regExp .>>. regExpAux |>> Union
                    regExp .>>. regExpAux |>> ConcatAux 
                    ws |>> konst Epsilon] 
    
        let regularExpression = regExp 

    let rec regularExp () = (unionRE () |>> Union) <|> (simpleRE () |>> Simple)
    and unionRE () = regularExp () .>>. ((pchar '+')  >>. simpleRE ())
    and simpleRE () = (concatRE |>> Concat) <|> (basicRE |>> Basic)
    and concatRE<'a> : Parser<ConcatRE, 'a> = simpleRE () .>>. basicRE
    and basicRE<'a> : Parser<BasicRE, 'a> = 
        (elementaryRE .>> (pchar '*') |>> Star) 
        <|> (elementaryRE |>> Elementary)
    and elementaryRE<'a> : Parser<ElementaryRE, 'a> = 
        (pchar '.' |>> konst Any) <|> (groupRE |>> Group)
    and groupRE<'a> : Parser<GroupRE, 'a> = 
        pchar '(' >>. regularExp () .>> pchar ')' 
    
    let pregExp<'a> : Parser<RegularExp, 'a> = regularExp ()

    // do regExpRef :=
    //     choice [
    //             // regExp .>> pchar '*' |>> StarExp 
    //             pipe2 regExp (ws >>. pchar '+' >>. regExp) (curry OrExp)
    //             // pipe2 regExp (ws >>. regExp) (curry AndExp)
    //             pchar '.' |>> konst AnyExp]
    //             // nodeConstraint]
      
    let rec regularConstraint<'a> : Parser<RegularConstraint, 'a> =
        pipe2 pregExp
              (betweenChars '(' ')' (many id))
              (curry RegularConstraint)

    let private optionally ret p = p <|> (ws |>> konst ret)

    let parseQuery<'a> : Parser<Query, 'a> = 
        pipe3 (pstring "MATCH" >>. ws >>. (manyWith id "NODES")) // parse nodes
              (manyWith pathConstraint "SUCH THAT" |> optionally []) // path constraints
              (manyWith regularConstraint "WHERE" |> optionally [])  // path constraints
              (fun nodes pathConstrs regularConstrs -> 
                    { Query.empty with 
                        nodes              = nodes 
                        pathConstraints    = pathConstrs 
                        regularConstraints = regularConstrs })
        // .>> ws
        
