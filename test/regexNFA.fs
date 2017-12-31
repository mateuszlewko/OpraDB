namespace OpraDB.Test

open OpraDB.RegexNFA
open OpraDB.LangTypes
open Expecto
open MBrace.FsPickler

module RegexNFA = 
    let private ser = FsPickler.CreateXmlSerializer (indent = true)
    /// removes whitespace from string 
    let noWS = String.filter (System.Char.IsWhiteSpace >> not)

    [<Tests>]
    let ``state machine tests (ofRegExp) `` =
        testList "correct output" [
            test "simple test with concats" {
                let regexAst = 
                    ConcatExp 
                        (AnyExp, // .
                         ConcatExp 
                            (StarExp AnyExp, // .*
                             ConcatExp
                                (AnyExp,    // . 
                                 AnyExp)))  // .

                let nfa = State.ofRegExp regexAst 
                let nfsStr = ser.PickleToString nfa
                Expect.equal (noWS nfsStr)
                    <| noWS """<?xml version="1.0" encoding="utf-16"?>
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
                    </FsPickler>"""
                <| "serialized representation differs"
            }
        ]