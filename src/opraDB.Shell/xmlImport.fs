namespace OpraB.Shell

open FSharp.Data
open FSharpx
open FSharpx.Collections
open OpraDB.AST
// open Hekate
// open FSharp.Data
open OpraDB.Data

module XmlImport = 
    
    type GraphML = 
        XmlProvider<
            """ <graphml>
                   <node id='1'>
                     <data key='labelV'>airport</data>
                     <data key='type'>airport</data>
                     <data key='code'>ATL</data>
                     <data key='icao'>KATL</data>
                     <data key='city'>Atlanta</data>
                     <data key='desc'>Hartsfield - Jackson Atlanta ...</data>
                     <data key='region'>US-GA</data>
                     <data key='runways'>5</data>
                     <data key='longest'>12390</data>
                     <data key='elev'>1026</data>
                     <data key='country'>US</data>
                     <data key='lat'>33.6366996765137</data>
                     <data key='lon'>-84.4281005859375</data>
                   </node>
                   <node id='2'>
                     <data key='labelV'>airport</data>
                     <data key='type'>airport</data>
                     <data key='code'>ATL</data>
                     <data key='icao'>KATL</data>
                     <data key='city'>Atlanta</data>
                     <data key='desc'>Hartsfield - Jackson Atlanta ...</data>
                     <data key='region'>US-GA</data>
                     <data key='runways'>5</data>
                     <data key='longest'>12390</data>
                     <data key='elev'>1026</data>
                     <data key='country'>US</data>
                     <data key='lat'>33.6366996765137</data>
                     <data key='lon'>-84.4281005859375</data>
                   </node>
                   <edge id='53374' source='3615' target='2982'>
                     <data key='labelE'>contains</data>
                   </edge>
                   <edge id='53375' source='3614' target='2983'>
                     <data key='labelE2'>contains</data>
                     <data key='labelE'>contains</data>
                   </edge>
                </graphml>
            """, Global=true>
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
        try let g = GraphML.Parse str
            parseNodes g.Nodes, parseEdges g.Edges
        with _ -> 
            failwith "Failed to parse graph data"

    let importGraph str = 
        parseGraph str |> uncurry buildGraph 