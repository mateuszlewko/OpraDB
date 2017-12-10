namespace OpraDB

open OpraDB.LangTypes
open FParsec
open FSharpx.Functional.Prelude
open FSharp.Core

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

    /// example: "string"
    let private stringLiteral<'a> : Parser<_, 'a> =
        between (str "\"") (str "\"")
                (manySatisfy ((<>) '"'))
        <??> "string literal in double quotes"
        |>> StringLiteral

    let private manyWith elem prefix = 
        pstring prefix >>. ws >>. pchar '(' >>. ws // prefix and opening bracket
        >>. (many elem)                            // list of elements
        .>> (pchar ')' .>> ws)                     // closing bracket

    let private betweenChars first last = 
        between (pchar first) (pchar last)
 
    /// Parse @x or @'x where x is integer
    let private nodeVar<'a> : Parser<_, 'a> = 
        ws >>. choice [pstring "@'" >>. pint32 |>> NextNodeVar
                       pchar '@'    >>. pint32 |>> CurrNodeVar] .>> ws

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
        // TODO: Add node constraint which returns bool (only one operand in expression)
        pipe3 operand operator operand 
              (curry3 NodeConstraint)
        |> betweenChars '[' ']'

    /// Used for debugging parser
    let private (<!>) (p: Parser<_,_>) label : Parser<_,_> = 
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply

    [<AutoOpen>]
    module RegexParser = 

        type private RegExpTail = 
            | Concat of RegExpTail * RegExpAux
            | Any of RegExpAux
            | Star of RegExpAux 
            | NodeCon of NodeConstraint * RegExpAux

        and private RegExpAux = 
            | Union of RegExpTail 
            | Tail of RegExpTail 
            | Epsilon

        // Added in order to use mutually recursive parsers
        let private reg = createParserForwardedToRef<RegExpTail, unit> ()
        let private regAux = createParserForwardedToRef<RegExpAux, unit> ()
        let private regExp = fst reg 
        let private regExpAux = fst regAux

        snd reg :=
            choice [betweenChars '(' ')' regExp .>>. regExpAux |>> Concat
                    pchar '.' >>. regExpAux |>> Any
                    pchar '*' >>. regExpAux |>> Star 
                    nodeConstraint .>>. regExpAux |>> NodeCon]
            |> between ws ws

        snd regAux := 
            choice [pchar '+' >>. regExp |>> Union
                    regExp |>> Tail 
                    ws |>> konst Epsilon]
            |> between ws ws

        exception ParsingException of string

        let private toConcats stack =
            let rec concats =
                function
                | [] -> EpsilonExp
                | x::xs -> ConcatExp (x, concats xs)

            stack |> List.rev |> concats
            
        let rec private parseAux (stack : RegularExpression list) = 
            function 
            | Tail reg -> parseReg stack reg
            | Epsilon -> toConcats stack
            | Union reg -> UnionExp (toConcats stack, parseReg [] reg)

        and private parseReg stack = 
            let (!!) s = raise (ParsingException s)

            function
            | Star aux -> 
                match stack with 
                | StarExp _ :: stack -> !! "'**' is an invalid expression"
                | term :: stack      -> parseAux (StarExp term :: stack) aux
                | []                 -> !! "The token preceding '*' is not quantifiable"
            | Concat (reg, aux)     -> parseAux (parseReg [] reg::stack) aux
            | Any aux               -> parseAux (AnyExp::stack) aux
            | NodeCon (constr, aux) -> parseAux (NodeExp constr::stack) aux

        let regularExpression = 
            regExp |>> parseReg []
      
    let rec private regularConstraint =
        pipe2 regularExpression
              (betweenChars '<' '>' (many id) .>> ws)
              (curry RegularConstraint)

    let private optionally ret p = p <|> (ws |>> konst ret)

    let query = 
        pipe3 (pstring "MATCH" >>. ws >>. (manyWith id "NODES")) // parse nodes
              (manyWith pathConstraint "SUCH THAT" |> optionally []) // path constraints
              (manyWith regularConstraint "WHERE" |> optionally []) // regular constraints
              (fun nodes pathConstrs regularConstrs -> 
                    { Query.empty with 
                        nodes              = nodes 
                        pathConstraints    = pathConstrs 
                        regularConstraints = regularConstrs })
        .>> ws