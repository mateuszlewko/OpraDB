open OpraDB.QueryExecution
open OpraDB.Data
open FSharpx.Functional
open FSharp.Data
open Argu
open System.IO
open System

exception ParsingErrorException of string

type DBDataFormat = 
    | Json = 1
    // | Csv = 2 // TODO: Add csv data format

type Arguments =
    | [<ExactlyOnce>] Input_Data of path:string
    | Input_Data_Format of format:DBDataFormat
with
    interface IArgParserTemplate with
        member this.Usage = 
            match this with 
            | Input_Data _        -> "specify input data file path."
            | Input_Data_Format _ -> "format of input file (json or csv)."

let parseGraph str = 
    let pExn str = raise (ParsingErrorException str)
    let parseProperty name label = 
        match label with 
        | JsonValue.Number i -> IntVal (int i)
        | JsonValue.String s -> StringVal s
        | t                  -> pExn (sprintf "Unexpected property type: %A." t)

    let parseNode = 
        function
        | JsonValue.Record properties ->
            let properties = Map.ofArray properties

            match Map.tryFind "_id" properties with 
            | Some (JsonValue.Number id) -> 
                int id, Map.map parseProperty properties
            | _ -> pExn "Node is missing '_id' field."
        | _ -> pExn "Node is expected to be a record."

    let parseEdge = 
        function
        | JsonValue.Record properties ->
            let properties = Map.ofArray properties
            let prop p     = Map.tryFind p properties

            match prop "_from", prop "_to" with 
            | Some (JsonValue.Number from), Some (JsonValue.Number to') -> 
                int from, int to', Map.map parseProperty properties
            | _ -> pExn "Edge is missing '_from' or '_to' field."
        | _ -> pExn "Edge is expected to be a record."

    match JsonValue.Parse str with 
    | JsonValue.Record properties ->
        let properties = Map.ofArray properties 
        let prop p     = Map.tryFind p properties

        match prop "edges", prop "nodes" with 
        | Some (JsonValue.Array edges), Some (JsonValue.Array nodes) -> 
           let map f = Array.map f >> Array.toList
           map parseNode nodes, map parseEdge edges
        | _ -> pExn "Missing 'nodes' or edges field."
    | _ -> pExn "Expected json record on top level." 

let importGraph str = parseGraph str |> uncurry buildGraph 

let eval graph str = 
    execute graph (OpraDB.Parser.parseQuery str)

let run args = 
    let parser  = ArgumentParser.Create<Arguments> (programName = "opradb")
    let results = 
        try parser.Parse args 
        with :? ArguParseException as e ->
            printfn "%s" e.Message
            exit 0

    let graph = results.GetResult Input_Data |> File.ReadAllText |> importGraph 

    let rec loop currStr =
        let l = Console.ReadLine ()

        if isNull l 
        then printfn "Goodbye."
        elif (l.TrimEnd ()).EndsWith ";"
        then try eval graph (currStr + (l.[0 .. String.length l - 1]))
             with e -> printfn "There was an exception: %A" e
                       exit 0
 
             printf "\n> "
             loop ""
        else loop (currStr + "\n" + l)

    // printfn "Graph is: %A" graph
    printf "> "
    loop ""

[<EntryPoint>]
let main args =
    printfn "  /$$$$$$                                  /$$$$$$$ /$$$$$$$ 
 /$$__  $$                                | $$__  $| $$__  $$
| $$  \ $$ /$$$$$$  /$$$$$$ /$$$$$$       | $$  \ $| $$  \ $$
| $$  | $$/$$__  $$/$$__  $|____  $$      | $$  | $| $$$$$$$ 
| $$  | $| $$  \ $| $$  \__//$$$$$$$      | $$  | $| $$__  $$
| $$  | $| $$  | $| $$     /$$__  $$      | $$  | $| $$  \ $$
|  $$$$$$| $$$$$$$| $$    |  $$$$$$$      | $$$$$$$| $$$$$$$/
 \______/| $$____/|__/     \_______/      |_______/|_______/ 
         | $$                                                
         | $$                                                
         |__/                                                "

    run args

    0