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

    let tryParse query = 
        let lexbuf = LexBuffer<char>.FromString query
        try Grammar.query Lexer.read lexbuf |> Ok
        with _ -> 
            sprintf "Parsing error from %A to %A.\n" lexbuf.StartPos 
                                                     lexbuf.EndPos
            + (sprintf "On token: %s\n" (System.String.Concat(lexbuf.Lexeme)))
            + (sprintf "Line %d, col %d\n" lexbuf.StartPos.Line 
                                           lexbuf.StartPos.Column)
            |> Error
            