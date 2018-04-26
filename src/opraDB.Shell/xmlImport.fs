namespace OpraDB.Shell

open FSharp.Data
open FSharpx
open FSharpx.Collections
open OpraDB.AST
// open Hekate
// open FSharp.Data
open OpraDB.Data
// open OpraDB.Shell.XmlParser

module XmlImport = 
    
    type GraphML = 
        XmlProvider<"sample.graphml", Global=true>
        // XmlProvider<
        //     """ <graphml>
        //            <graph id='routes' edgedefault='directed'> 
        //            <key id='type'    for='node' attr.name='type'    attr.type='string'></key>
        //            <key id='code'    for='node' attr.name='code'    attr.type='string'></key>
        //            <key id='icao'    for='node' attr.name='icao'    attr.type='string'></key>
        //            <key id='desc'    for='node' attr.name='desc'    attr.type='string'></key>
                  
        //            <node id='1'>
        //              <data key='labelV'>airport</data>
        //              <data key='type'>airport</data>
        //              <data key='code'>ATL</data>
        //              <data key='icao'>KATL</data>
        //              <data key='city'>Atlanta</data>
        //              <data key='desc'>Hartsfield - Jackson Atlanta ...</data>
        //              <data key='region'>US-GA</data>
        //              <data key='runways'>5</data>
        //              <data key='longest'>12390</data>
        //              <data key='elev'>1026</data>
        //              <data key='country'>US</data>
        //              <data key='lat'>33.6366996765137</data>
        //              <data key='lon'>-84.4281005859375</data>
        //            </node>
        //            <node id='2'>
        //              <data key='labelV'>airport</data>
        //              <data key='type'>airport</data>
        //              <data key='code'>ATL</data>
        //              <data key='icao'>KATL</data>
        //              <data key='city'>Atlanta</data>
        //              <data key='desc'>Hartsfield - Jackson Atlanta ...</data>
        //              <data key='region'>US-GA</data>
        //              <data key='runways'>5</data>
        //              <data key='longest'>12390</data>
        //              <data key='elev'>1026</data>
        //              <data key='country'>US</data>
        //              <data key='lat'>33.6366996765137</data>
        //              <data key='lon'>-84.4281005859375</data>
        //            </node>
        //            <edge id='53374' source='3615' target='2982'>
        //              <data key='labelE'>contains</data>
        //            </edge>
        //            <edge id='53375' source='3614' target='2983'>
        //              <data key='labelE2'>contains</data>
        //              <data key='labelE'>contains</data>
        //            </edge>
        //            </graph>
        //         </graphml>
        //     """, Global=true>

    let inline parseData (d : GraphML.Data) =    
        match d.Number, d.String with 
        | Some n, _    ->
            let ns = string n
            if String.contains "," ns ||
               String.contains "." ns 
            then float n |> Float 
            else int n   |> Int 
            |> fun v -> Some (d.Key, v)
        | None, Some s -> Some (d.Key, String s)
        | None, None   -> None 
     
    let parseDatas = Array.choose parseData >> Map.ofArray

    let parseNodes (nodes : GraphML.Node array) = 
        let parseNode (node : GraphML.Node) = 
            node.Id, parseDatas node.Datas 
        Array.map parseNode nodes |> Array.toList

    let parseEdges (edges : GraphML.Edge array) = 
        let parseEdge (edge : GraphML.Edge) = 
            let labels = parseDatas edge.Datas
            edge.Source, edge.Target, labels
        Array.map parseEdge edges |> Array.toList

    let parseGraph str =
        try let g = (GraphML.Parse str).Graph
            // printfn "nodes: %A" g.Nodes
            // printfn "edges: %A" g.Edges
            parseNodes g.Nodes, parseEdges g.Edges
        with e -> 
            failwithf "Failed to parse graph data: %A" e.Message

    let importGraph str = 
        parseGraph str |> uncurry buildGraph 