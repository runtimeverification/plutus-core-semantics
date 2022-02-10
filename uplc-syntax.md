
# The Concrete Syntax of Untyped Plutus

```k
require "domains.md"
require "bytestring.md"

module UPLC-CONCRETE-SYNTAX
  imports LIST
  imports ID
  imports INT-SYNTAX
  imports BYTESTRING-SYNTAX

  syntax Program ::= "(" "program" Version Term ")"     // versioned program

  syntax Version ::= r"[0-9]+.[0-9]+.[0-9]+" [token]

  syntax Term ::= Id
                | Value
                | "[" Term Term "]"                     // function application
                | "(" "force" Term ")"                  // force execution of a term
                | "(" "builtin" BuiltinName ")"
                | "(" "builtin" BuiltinName TermList ")"// builtin
                | "(" "error" ")"                       // error

  syntax Value ::= "(" "con" TypeConstant Constant ")"
                 | "(" "lam" Id Term ")"
                 | "(" "delay" Term ")"

  syntax TermList ::= NeList{Term, ""}

  syntax TypeConstant ::= "integer"
                        | "data"
                        | "bytestring"
                        | "unit"
                        | "bool"

  syntax Constant ::= Int
                    | "True"
                    | "False"
                    | ByteString
                    | "()"

```

### builtin functions for Integers

```k
  syntax BuiltinName ::= "addInteger"
                       | "subtractInteger"
                       | "multiplyInteger"
                       | "divideInteger"
                       | "quotientInteger"
                       | "remainderInteger"
                       | "modInteger"
                       | "equalsInteger"
                       | "lessThanInteger"
                       | "lessThanEqualsInteger"
```

### builtin functions for bytestring

```k
  syntax BuiltinName ::= "appendByteString"
                       | "consByteString"
                       | "sliceByteString"
                       | "lengthOfByteString"
                       | "indexByteString"
                       | "equalsByteString"
                       | "lessThanByteString"
                       | "lessThanEqualsByteString"
```

### builtin functions for cryptography

```k
  syntax BuiltinName ::= "sha2_256"
                       | "sha3_256"
                       | "blake2b_256"
                       | "verifySignature"
```

### builtin functions for string

```k
  syntax BuiltinName ::= "appendString"
                       | "equalsString"
                       | "encodeUtf8"
                       | "decodeUtf8"
```

### polymorphic builtin functions

```k
  syntax BuiltinName ::= "ifThenElse"
                       | "chooseUnit"
                       | "trace"
```

### builtin functions that operate on pairs

```k
  syntax BuiltinName ::= "fstPair"
                       | "sndPair"
```

### builtin functions that operate on lists

```k
  syntax BuiltinName ::= "chooseList"
                       | "mkCons"
                       | "headList"
                       | "tailList"
                       | "nullList"
```

### builtin functions that operate on data

```k
  syntax BuiltinName ::= "chooseData"
                       | "constrData"
                       | "mapData"
                       | "listData"
                       | "iData"
                       | "bData"
                       | "unConstrData"
                       | "unMapData"
                       | "unListData"
                       | "unIData"
                       | "unBData"
                       | "equalsData"
                       | "mkPairData"
                       | "mkNilData"
                       | "mkNilPairData"
endmodule
```

# The Abstract Syntax of Untyped Plutus

These constructs are used in the semantics to describe partially applied builtin functions.
```k
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
