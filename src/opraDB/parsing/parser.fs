namespace OpraDB

open Microsoft.FSharp.Text.Lexing

module Parser =
    let parseQuery query = 
        let lexbuf = LexBuffer<char>.FromString query
        try Grammar.query Lexer.read lexbuf
        with _ ->
            printfn "Parsing error from %A to %A." lexbuf.StartPos lexbuf.EndPos
            printfn "On token: %s" (System.String.Concat(lexbuf.Lexeme))
            printfn "Line %d, col %d" lexbuf.StartPos.Line lexbuf.StartPos.Column
            reraise ()
