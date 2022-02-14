
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

### Builtin Functions for Integers

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

### Builtin Functions for bytestring

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

### Builtin Functions for Cryptography

```k
  syntax BuiltinName ::= "sha2_256"
                       | "sha3_256"
                       | "blake2b_256"
                       | "verifySignature"
```

### Builtin Functions for String

```k
  syntax BuiltinName ::= "appendString"
                       | "equalsString"
                       | "encodeUtf8"
                       | "decodeUtf8"
```

### Polymorphic Builtin Functions

```k
  syntax BuiltinName ::= "ifThenElse"
                       | "chooseUnit"
                       | "trace"
```

### Builtin Functions that Operate on Pairs

```k
  syntax BuiltinName ::= "fstPair"
                       | "sndPair"
```

### Builtin Functions that Operate on Lists

```k
  syntax BuiltinName ::= "chooseList"
                       | "mkCons"
                       | "headList"
                       | "tailList"
                       | "nullList"
```

### Builtin Functions that Operate on Data

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
                 | "#SHA2" // for sha2_256
                 | #SHA2(Value)
                 | "#SHA3" // for sha3_256
                 | #SHA3(Value)
                 | "#BLK2B" // for blake2b_256
                 | #BLK2B(Value)

endmodule

module UPLC-SYNTAX
  imports UPLC-CONCRETE-SYNTAX
  imports UPLC-ABSTRACT-SYNTAX
endmodule
```
