open OpraDB.Interpreter
open OpraDB.Parser
open OpraDB.LangTypes
open OpraDB.RegexNFA
open FParsec 
open MBrace.FsPickler

let test p str =
    printf "%s => " str
    match run p str with
    | Success (result, _, _)   -> printfn "Success: %A" result
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let parseAndRun str = 
    match run query str with 
    | Success (result, _, _)   -> printfn "Success: %A" result; interpret result 
    | Failure (errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test regularExpression ".(.+..*)+.."
    test regularExpression ".*"
    test regularExpression "(.*..)+."
    test regularExpression "([attr(@1) > 10]*.)"
    test regularExpression ".*..."
    test regularExpression "(..)*.."
    test regularExpression ".*.+."
    test regularExpression ".*.+.+.."
    test regularExpression "...(.(..)*.+.)...."

    let q1 = "MATCH NODES (s t x y)
              SUCH THAT (s-[p1]->t x-[p2]->y)"

    parseAndRun q1

    let q2 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t)
              WHERE (..*<p>)"

    parseAndRun q2

    let q3 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t)
              WHERE ([attr(@1) >10] *. +..( .*)<p>)"

    parseAndRun q3

    let q4 = "MATCH NODES (s t)
              SUCH THAT (s-[p]->t s-[p2]->t)
              WHERE (.*[type (@1 @'1) = \"some type\"] *.<p>  .+..(.*)..<p p2>)"

    parseAndRun q4

    let q5 =  "MATCH NODES (s t)
               WHERE (.*[type (@1 @'1) = \"some type\"]*.<p> 
                      .+(..(.*))..<p p2> )" 
    parseAndRun q5

    let regexAst = 
        ConcatExp 
            (AnyExp, // .
             ConcatExp 
                (StarExp AnyExp, // .*
                 EpsilonExp))

    State.ofRegExp regexAst |> printfn "%A"

    let regexAst = 
        ConcatExp 
            (AnyExp, // .
             ConcatExp 
                (StarExp AnyExp, // .*
                 ConcatExp
                    (AnyExp,    // . 
                     AnyExp)))  // .

    let nfa = State.ofRegExp regexAst 
    nfa |> printfn "%A"

    let ser = FsPickler.CreateXmlSerializer (indent = true)
    printfn "Serialized: %s\n" (ser.PickleToString nfa)
(*
<?xml version="1.0" encoding="utf-16"?>
<FsPickler version="4.0.0.0" type="OpraDB.RegexNFA+State">
  <value>
    <Case>Any</Case>
    <Item>
      <next>
        <Case>Empty</Case>
        <Item>
          <next>
            <Case>Any</Case>
            <Item>
              <next>
                <Case>Any</Case>
                <Item>
                  <next>
                    <Case>Matched</Case>
                  </next>
                  <nextAlt flags="null" />
                </Item>
              </next>
              <nextAlt flags="null" />
            </Item>
          </next>
          <nextAlt>
            <Some>
              <Case>Any</Case>
              <Item>
                <next flags="cyclic" id="3" />
                <nextAlt flags="null" />
              </Item>
            </Some>
          </nextAlt>
        </Item>
      </next>
      <nextAlt flags="null" />
    </Item>
  </value>
</FsPickler>
*)
    0
