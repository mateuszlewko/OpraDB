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
            test "one attribute - solution exists" {
                let ll p v = Labelling (ID p, [CurrNodeVar (ID v)])
                let lb1    = ll "p1" "a1"
                let constrs = [ Sum lb1 |> Ext, Leq, (Lit (Int 10))
                                Sum lb1 |> Ext, Geq, (Lit (Int 5)) ]
                              |> List.map (BoolOp >> AC.Value)
                let cyclesDeltas = [Map [lb1, Int 4]]                

                let res = existsSolution constrs Map.empty cyclesDeltas 
                Expect.isTrue res "found solution correctly"
            }

            test "two attributes - solution exists" {
                let ll p v = Labelling (ID p, [CurrNodeVar (ID v)])
                let lb1    = ll "p1" "a1"
                let lb2    = ll "p1" "a2"
                let constrs = [ Sum lb1 |> Ext, Leq, (Lit (Int 10))
                                Sum lb2 |> Ext, Geq, (Lit (Int 5)) ]
                              |> List.map (BoolOp >> AC.Value)
                let cyclesDeltas = [Map [lb1, Int 6 ; lb2, Int 3]
                                    Map [lb1, Int -3; lb2, Int -1]]                

                let res = existsSolution constrs Map.empty cyclesDeltas 
                Expect.isTrue res "found solution correctly"
            }
        ]