namespace OpraDB.Test

open Expecto
open OpraDB.Graph.Utils
open OpraDB.AST
open OpraDB.ArithmeticConstraints

module ArithmeticConstraints = 
        
    [<Tests>]
    let ``restore graph tests`` =
        testList "restored graph is correct" [
            test "handle cycles" {
                let preds = Map.ofList [1, Set.ofList [2]; 2, Set.ofList [3; 5]
                                      ; 3, Set.ofList [4]; 4, Set.ofList [2; 5]
                                      ; 5, Set.ofList [4]]

                let graph, visited = restoreGraph preds 1

                Expect.equal (allSimpleCycles graph) [[3; 4; 2]; [5; 4; 2]] 
                             "finds all cycles"

                Expect.equal ([(2, 1, ()); (2, 4, ()); (3, 2, ()); (4, 3, ()); 
                               (4, 5, ()); (5, 2, ()); (5, 4, ())]) 
                             (Hekate.Graph.Edges.toList graph) "same edges"
            }
        ]
        
    [<Tests>]
    let ``exists solution tests`` =
        testList "exists solution returns correct output" [
            test "basic test" {
                let constrs = [ SumBy (ID "p1", ID "a1"), Leq, IntALiteral 10
                                SumBy (ID "p1", ID "a1"), Geq, IntALiteral 5 
                              ] |> List.map ArithmeticConstraint 
                let cyclesDeltas = [Map [(ID "p1", ID "a1"), 3]]                

                let res = existsSolution constrs cyclesDeltas
                Expect.isTrue res "found solution correctly"
            }
        ]