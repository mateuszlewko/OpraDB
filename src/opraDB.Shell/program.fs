open OpraDB.QueryExecution
open OpraDB.Data
open FSharpx.Functional
open FSharp.Data
open Argu

exception ParsingErrorException of string

type DBDataFormat = 
    | Json = 1
    // | Csv = 2

type Arguments =
    | [<ExactlyOnce>] Input_Data of path:string
    | Input_Data_Format of format:DBDataFormat
    interface IArgParserTemplate with
        member s.Usage = "info."

let parseGraph str = 
    match JsonValue.Parse str with 
    | JsonValue.Record [|"nodes", nodes; "edges", edges|] ->
        0
    | _ -> "Missing 'nodes' or edges field or some unexpected field."
           |> ParsingErrorException |> raise

    // TODO: Graph parsing
    (* 
    {
        "nodes": [
            {
                "_id": 1,
                "asas": 1
                "asss": "asas"
            }, { ... }
        ],
        "edges": [
            { "_from": 1,
              "_to": 1,
              "etykieta": "aaa"
              "val": 33
             }
        ]
    }
    *)
    [], []

let importGraph str = parseGraph str |> uncurry buildGraph 

let eval graph str = 
    printfn "eval."
    ()

let run args = 
    let parser  = ArgumentParser.Create<Arguments> (programName = "opradb")
    let results = 
        try parser.Parse args 
        with :? ArguParseException as e ->
            printfn "%s" e.Message
            exit 0

    let graph = importGraph "" // TODO: args parsing

    while true do 
        eval graph "" // TODO: input reading
        exit 0

[<EntryPoint>]
let main args =
    printfn "*** OpraDB Shell ***"
    run args

    0