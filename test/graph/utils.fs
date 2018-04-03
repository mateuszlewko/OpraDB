namespace OpraDB.Test

open Expecto
open OpraDB.Graph.Utils

module ArithmeticConstraints = 
        
    [<Tests>]
    let ``restore graph tests`` =
        testList "restored graph is correct" [
            test "handle cycles" {
                let preds = Map.ofList [1, Set.ofList [2]; 2, Set.ofList [3; 5]
                                      ; 3, Set.ofList [4]; 4, Set.ofList [2; 5]
                                      ; 5, Set.ofList [4]]

                let graph, visited =  restoreGraph preds 1
                // printfn "post order %A" (postOrderTimes graph)
                // printfn "sccs %A" (stronglyConnectedComponents graph)

                OpraDB.ArithmeticConstraints.existsSolution 1 1


                Expect.equal (allSimpleCycles graph) [[3; 4; 2]; [5; 4; 2]] 
                             "finds all cycles"

                Expect.equal ([(2, 1, ()); (2, 4, ()); (3, 2, ()); (4, 3, ()); 
                               (4, 5, ()); (5, 2, ()); (5, 4, ())]) 
                             (Hekate.Graph.Edges.toList graph) "same edges"
            }
        ]