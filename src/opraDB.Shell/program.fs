open OpraDB.QueryExecution
open FSharpx
open Argu
open System.IO
open System
open Hekate
open PrettyTable
open FSharpx.Collections
open FSharpx.Option
open OpraDB.AST
open OpraDB.Shell

type DBDataFormat = 
    | Json = 1
    | Xml  = 2

type Arguments =
    | [<ExactlyOnce>] Input_Data of path:string
    | [<ExactlyOnce>] Input_Data_Format of format:DBDataFormat
with
    interface IArgParserTemplate with
        member this.Usage = 
            match this with 
            | Input_Data _        -> "specify input data file path."
            | Input_Data_Format _ -> "format of input file (json or \
                                      graphml-xml)."

type NodeResult = 
    | ID       of int 
    | LabelLit of Literal

let mapListToPTable (ms : Map<OpraDB.AST.NodeMatched, NodeResult> list) =
    let nodeMatchedToStr = 
        function 
        | NodeID (Identifier.ID i) -> i 
        | NodeLabel (label, Identifier.ID i) -> sprintf "%s(%s)" label i

    let nodeResToStr = 
        function 
        | ID i                -> sprintf "%d" i 
        | LabelLit (Int i)    -> sprintf "%d" i 
        | LabelLit (Bool b)   -> sprintf "%A" b
        | LabelLit (Float f)  -> sprintf "%f" f
        | LabelLit (String s) -> s
        | LabelLit (Null)     -> "<null>"

    let headers = List.collect (Map.keys >> List.ofSeq) ms 
                  |> List.distinct
    let data    = 
        let get = Option.map nodeResToStr >> Option.defaultValue "<null>"  
        List.map (fun m -> List.map (flip Map.tryFind m >> get) headers) ms 

    prettyTable data |> withHeaders (List.map nodeMatchedToStr headers)
    
let eval graph str = 
    let query = OpraDB.Parser.parseQuery str
    // printfn "query: %A" query
    let nodes = matchedNodes graph query
    
    // let findLabel n l =
    if List.isEmpty nodes then printfn "<empty>"
    else nodes 
         |> List.map (fun map -> 
                List.choose (
                    function 
                    | NodeID i as n -> Map.tryFind i map 
                                       |> Option.map (fun v -> n, ID v)
                    | NodeLabel (l, i) as n -> 
                        maybe {
                            let! v           = Map.tryFind i map 
                            let! (_, labels) = Graph.Nodes.tryFind v graph 
                            let! label       = Map.tryFind l labels 
                            return n, LabelLit label
                        }
                ) query.basic.nodes |> Map.ofList
            ) 
         |> mapListToPTable
         |> printTable

let run args = 
    let parser  = ArgumentParser.Create<Arguments> (programName = "opradb")
    let results = 
        try parser.Parse args 
        with :? ArguParseException as e ->
            printfn "%s" e.Message
            exit 0

    let fmt = results.GetResult Input_Data_Format
    let importGraph = 
        results.GetResult Input_Data_Format 
         |> function 
            | DBDataFormat.Json -> JsonImport.importGraph
            | DBDataFormat.Xml  -> XmlImport.importGraph
            | _                 -> failwith "unknown data format"

    let graph = results.GetResult Input_Data |> File.ReadAllText |> importGraph 
     
    printfn "Loaded graph."

    let rec loop currStr =
        let l = Console.ReadLine ()
        
        if isNull l 
        then printfn "Goodbye."
        elif (l.TrimEnd ()).EndsWith ";"
        then try printfn ""
                 eval graph (currStr + " " + (l.[0 .. String.length l - 1]))
             with e -> printfn "There was an exception: %A" e
 
             printf "\n> "
             loop ""
        else loop (currStr + " \n " + l)

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