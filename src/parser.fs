namespace OpraDB

open Microsoft.FSharp.Text.Lexing

module Parser =
    let parseQuery query = 
        let lexbuf = LexBuffer<char>.FromString query
        try Grammar.query Lexer.read lexbuf
        with _ ->
            printfn "Parsing error from %A to %A." lexbuf.StartPos lexbuf.EndPos
            reraise ()
