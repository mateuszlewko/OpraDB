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