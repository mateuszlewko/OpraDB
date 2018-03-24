namespace OpraDB

module CommonUtils =

    let inline internal fst3 (a,_,_) = a
    let inline internal snd3 (_,a,_) = a
    let inline internal thr3 (_,_,a) = a

    module List = 
        let rec cartesian =
            function
            | []   -> []
            | [h]  -> List.fold (fun acc elem -> [elem]::acc) [] h
            | h::t ->
                List.fold (fun cacc celem ->
                    (List.fold (fun acc elem -> (elem::celem)::acc) [] h) @ cacc
                ) [] (cartesian t)

    module MultiMap = 
        open FSharpx.Collections

        let add key value m = 
            match Map.tryFind key m with 
            | None   -> Set.singleton value 
            | Some s -> Set.add key s
            |> fun s -> Map.add key s m