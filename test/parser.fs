namespace OpraDB.Test

// open OpraDB.Parser
// open OpraDB.LangTypes
open Expecto
open FParsec

module Parser = 

    let successfulParse parser s = 
        match run parser s with 
        | Success (result, _, _) -> result
        | Failure (error, _, _)  -> 
            failtestf "Failed to parse: %s\nError message: %s" s error

    [<Tests>]
    let ``query tests`` =
        testList "query parsing" [
            test "parsing nodes match and path constrains" {
                skiptest "skip"
                // let ast = successfulParse query 
                //             "MATCH NODES (s t x y)
                //              SUCH THAT (s-[p1]->t x-[p2]->y)" 

                // Expect.equal ast { nodes = [ID "s"; ID "t"; ID "x"; ID "y"]
                //                    paths = []
                //                    pathConstraints = [{source  = ID "s"
                //                                        target  = ID "t"
                //                                        path    = ID "p1"}
                //                                       {source = ID "x"
                //                                        target = ID "y"
                //                                        path   = ID "p2"}]
                //                    regularConstraints = [] }
                //     "ast parsed correctly"
            }

            test "parsing nodes match, path constrains and \
                  regular constrains" {
                skiptest ""
                // let ast = successfulParse query 
                //             "MATCH NODES (s t)
                //              SUCH THAT (s-[p]->t)
                //              WHERE (..*<p>)" 

                // Expect.equal ast { nodes = [ID "s"; ID "t"]
                //                    paths = []
                //                    pathConstraints = [{source  = ID "s"
                //                                        target  = ID "t"
                //                                        path    = ID "p"}]
                //                    regularConstraints = 
                //                     [ConcatExp 
                //                          (AnyExp, // .
                //                           ConcatExp 
                //                              (StarExp AnyExp, // .*
                //                               EpsilonExp)) 
                //                      , [ID "p"] // <p>
                //                     ]}
                //     "ast parsed correctly"
            }

            test "parsing nodes match and multiple regular constrains \
                  with node constrains" {
                skiptest ""
                // let ast = successfulParse query 
                //             "MATCH NODES (s t)
                //              WHERE (.*[type (@1 @'1) = \"some type\"]*.<p> 
                //                     .+(..(.*))..<p p2> )" 

                // Expect.equal ast {nodes = [ID "s"; ID "t"];
                //                   paths = [];
                //                   pathConstraints = [];
                //                   regularConstraints =
                //                    [(ConcatExp
                //                       (StarExp AnyExp,
                //                        ConcatExp
                //                          (StarExp
                //                             (NodeExp
                //                                (NodeConstraint
                //                                   (Labelling (ID "type",[CurrNodeVar 1; NextNodeVar 1]),Eq,
                //                                    StringLiteral "some type"))),ConcatExp (AnyExp,EpsilonExp))),
                //                        [ID "p"]);
                //                     (UnionExp
                //                       (ConcatExp (AnyExp,EpsilonExp),
                //                        ConcatExp
                //                          (ConcatExp
                //                             (AnyExp,
                //                              ConcatExp
                //                                (AnyExp,
                //                                 ConcatExp (ConcatExp (StarExp AnyExp,EpsilonExp),EpsilonExp))),
                //                           ConcatExp (AnyExp,ConcatExp (AnyExp,EpsilonExp)))),[ID "p"; ID "p2"])];}

                //     "ast parsed correctly"
            }
        ]   
