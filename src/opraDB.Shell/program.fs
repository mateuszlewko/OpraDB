open OpraDB.QueryExecution
open FSharpx
open Argu
open System.IO
open System
open PrettyTable
open FSharpx.Collections
open OpraDB.AST
open OpraDB.Shell.JsonImport

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

let mapListToPTable ms =
    let hs   = List.collect (Map.keys >> List.ofSeq) ms |> List.distinct 
    let get  = Option.map (sprintf "%A") >> Option.defaultValue "<null>"  
    let data = List.map (fun m -> List.map (flip Map.tryFind m >> get) hs) ms 

    prettyTable data |> withHeaders hs
    
let eval graph str = 
    let query = OpraDB.Parser.parseQuery str
    // printfn "query: %A" query
    let nodes = matchedNodes graph query
    
    if List.isEmpty nodes then printfn "<empty>"
    else nodes 
      |> List.map (Map.toList >> List.map (fun (ID a, b) -> a, b) >> Map.ofList) 
      |> mapListToPTable
      |> printTable

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
        then try printfn ""
                 eval graph (currStr + (l.[0 .. String.length l - 1]))
             with e -> printfn "There was an exception: %A" e
 
             printf "\n> "
             loop ""
        else loop (currStr + "\n" + l)

    // printfn "Graph is: %A" graph
    printf "> "
    loop ""

[<EntryPoint>]
let main args =
    printfn "
Welcome to
  /$$$$$$                                  /$$$$$$$ /$$$$$$$ 
 /$$__  $$                                | $$__  $| $$__  $$
| $$  \ $$ /$$$$$$  /$$$$$$ /$$$$$$       | $$  \ $| $$  \ $$
| $$  | $$/$$__  $$/$$__  $|____  $$      | $$  | $| $$$$$$$ 
| $$  | $| $$  \ $| $$  \__//$$$$$$$      | $$  | $| $$__  $$
| $$  | $| $$  | $| $$     /$$__  $$      | $$  | $| $$  \ $$
|  $$$$$$| $$$$$$$| $$    |  $$$$$$$      | $$$$$$$| $$$$$$$/
 \______/| $$____/|__/     \_______/      |_______/|_______/ 
         | $$                                                
         | $$                                                
         |__/                                                \n"

    run args

    0