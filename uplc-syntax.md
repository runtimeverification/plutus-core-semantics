```k
require "domains.md"
require "bytestring.md"

module UPLC-CONCRETE-SYNTAX
  imports LIST
  imports ID
  imports INT-SYNTAX
  imports BYTESTRING-SYNTAX

  syntax TypeConstant ::= "integer"
                        | "data"
                        | "bytestring"
                        | "unit"
                        | "bool"

  syntax Constant     ::= Int
                        | "True"
                        | "False"
                        | ByteString
                        | "()"

  syntax BuiltinName  ::= "ifThenElse"
                        | "addInteger"
                        | "subtractInteger"
                        | "multiplyInteger"
                        | "divideInteger"
                        | "modInteger"
                        | "quotientInteger"
                        | "remainderInteger"
                        | "lessThanInteger"
                        | "lessThanEqualsInteger"
                        | "greaterThanInteger"
                        | "greaterThanEqualsInteger"
                        | "equalsInteger"
                        | "appendByteString"
                        | "consByteString"
                        | "sliceByteString"
                        | "lengthOfByteString"
                        | "indexByteString"
                        | "equalsByteString"
                        | "lessThanByteString"
                        | "lessThanEqualsByteString"

  syntax Value ::= "(" "con" TypeConstant Constant ")"
                 | "(" "lam" Id Term ")"
                 | "(" "delay" Term ")"

  syntax TermList ::= NeList{Term, ""}

  syntax Term ::= Id
                | Value
                | "[" Term Term "]"                     // function application
                | "(" "force" Term ")"                  // force execution of a term
                | "(" "builtin" BuiltinName ")"
                | "(" "builtin" BuiltinName TermList ")"// builtin
                | "(" "error" ")"                       // error

  syntax Version ::= r"[0-9]+.[0-9]+.[0-9]+" [token]

  syntax Program ::= "(" "program" Version Term ")"     // versioned program

endmodule

module UPLC-ABSTRACT-SYNTAX
  imports LIST
  imports ID
  imports INT-SYNTAX
  imports BYTESTRING-SYNTAX
  imports UPLC-CONCRETE-SYNTAX

  syntax Value ::= "#ITE"
                 | #ITE(Value)
                 | #ITE(Value, Value)
                 | #ITE(Value, Value, Value)
                 | "#SUM"
                 | #SUM(Value)
                 | #SUM(Value, Value)
                 | "#MUL"
                 | #MUL(Value)
                 | #MUL(Value, Value)
                 | "#SUB"
                 | #SUB(Value)
                 | #SUB(Value, Value)
                 | "#DIV"
                 | #DIV(Value)
                 | #DIV(Value, Value)
                 | "#MOD"
                 | #MOD(Value)
                 | #MOD(Value, Value)
                 | "#QUO"
                 | #QUO(Value)
                 | #QUO(Value, Value)
                 | "#REM"
                 | #REM(Value)
                 | #REM(Value, Value)
                 | "#LTI"
                 | #LTI(Value)
                 | #LTI(Value, Value)
                 | "#LTE"
                 | #LTE(Value)
                 | #LTE(Value, Value)
                 | "#GTI"
                 | #GTI(Value)
                 | #GTI(Value, Value)
                 | "#GTE"
                 | #GTE(Value)
                 | #GTE(Value, Value)
                 | "#EQI"
                 | #EQI(Value)
                 | #EQI(Value, Value)
                 | "#ABS" // for appendByteString
                 | #ABS(Value)
                 | #ABS(Value, Value)
                 | "#CBS" // for consByteString
                 | #CBS(Value)
                 | #CBS(Value, Value)
                 | "#SBS" // for sliceByteString
                 | #SBS(Value)
                 | #SBS(Value, Value)
                 | #SBS(Value, Value, Value)
                 | "#LBS" // for lengthOfByteString
                 | #LBS(Value)
                 | "#IBS" // for indexByteString
                 | #IBS(Value)
                 | #IBS(Value, Value)
                 | "#EBS" // for equalsByteString
                 | #EBS(Value)
                 | #EBS(Value, Value)
                 | "#LTBS" // for lesThanByteString
                 | #LTBS(Value)
                 | #LTBS(Value, Value)
                 | "#LEBS" // for lesThanEqualsByteString
                 | #LEBS(Value)
                 | #LEBS(Value, Value)

endmodule

module UPLC-SYNTAX
  imports UPLC-CONCRETE-SYNTAX
  imports UPLC-ABSTRACT-SYNTAX
endmodule
```
