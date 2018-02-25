namespace OpraDB

open FSharpx.Functional

open OpraDB.AST
open OpraDB.RegexNFA
open OpraDB.QueryData
open OpraDB.QueryData.MatchedEdge
open OpraDB.Data
open OpraDB.CommonUtils

open FSharpx.Collections
open FSharpx
open Hekate

module ArithmeticConstraints =

    let getValue (mKEdges : MatchedKEdges) 
                 (arithConstr : ArithmeticConstraint) =
        0

    let checkMatched (mKEdges : MatchedKEdges) 
                     (arithConstr : ArithmeticConstraint) =
        true
