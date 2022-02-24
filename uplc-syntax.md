# The Concrete Syntax of Untyped Plutus

```k
require "domains.md"
require "uplc-bytestring.md"

module UPLC-CONCRETE-SYNTAX
  imports LIST
  imports ID
  imports INT-SYNTAX
  imports UPLC-BYTESTRING
  imports STRING

  syntax ConcreteProgram ::= "(" "program" Version Term ")"
  syntax FlatProgram ::= ByteString

  syntax Program ::= ConcreteProgram
                 | FlatProgram
                 | #handleProgram(Program) [function]
                 | Bytes

  rule #handleProgram(C:ConcreteProgram) => C
  rule #handleProgram(F:FlatProgram) => String2Bytes(trimByteString({F}:>ByteString))

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
                        | "string"
                        | "unit"
                        | "bool"

  syntax Constant ::= Int
                    | "True"
                    | "False"
                    | ByteString
                    | String
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
  imports UPLC-CONCRETE-SYNTAX
```

## For `ifThenElse`

```k
  syntax Value ::= "#ITE"
                 | #ITE(Value)
                 | #ITE(Value, Value)
                 | #ITE(Value, Value, Value)
```                 
## For `addInteger`

```k
                 | "#SUM"
                 | #SUM(Value)
                 | #SUM(Value, Value)
```                 

## For `multiplyInteger`

```k
                 | "#MUL"
                 | #MUL(Value)
                 | #MUL(Value, Value)
```

## For `subtractInteger`

```k

                 | "#SUB"
                 | #SUB(Value)
                 | #SUB(Value, Value)
```                 

## For `divideInteger`

```k
                 | "#DIV"
                 | #DIV(Value)
                 | #DIV(Value, Value)
```                 

## For `modInteger`

```k
                 | "#MOD"
                 | #MOD(Value)
                 | #MOD(Value, Value)
```               

## For `quotientInteger`

```k
                 | "#QUO"
                 | #QUO(Value)
                 | #QUO(Value, Value)
```                 

## For `remainderInteger`

```k
                 | "#REM"
                 | #REM(Value)
                 | #REM(Value, Value)
```

## For `lessTahnInteger`

```k

                 | "#LTI"
                 | #LTI(Value)
                 | #LTI(Value, Value)
```

## For `lessThanEqualsInteger`

```k

                 | "#LTE"
                 | #LTE(Value)
                 | #LTE(Value, Value)
```

## For `equalsInteger`

```k
                 | "#EQI"
                 | #EQI(Value)
                 | #EQI(Value, Value)
```


## For `appendByteString`

```k
                 | "#ABS" 
                 | #ABS(Value)
                 | #ABS(Value, Value)
```                 

## For `consByteString`

```k
                 | "#CBS" 
                 | #CBS(Value)
                 | #CBS(Value, Value)
```

## For `sliceByteString`

```k
                 | "#SBS" // for sliceByteString
                 | #SBS(Value)
                 | #SBS(Value, Value)
                 | #SBS(Value, Value, Value)
```

## For `lengthOfByteString`

```k
                 | "#LBS" // 
                 | #LBS(Value)
```

## For `indexByteString`

```k
                 | "#IBS" 
                 | #IBS(Value)
                 | #IBS(Value, Value)
```

## For `equalsByteString`

```k
                 | "#EBS" 
                 | #EBS(Value)
                 | #EBS(Value, Value)
```

## For `lessThanByteString`

```k
                 | "#LTBS" 
                 | #LTBS(Value)
                 | #LTBS(Value, Value)
```                 

## For `lessThanEqualsByteString`

```k
                 | "#LEBS" 
                 | #LEBS(Value)
                 | #LEBS(Value, Value)
```

## For `sha2_256` 

```k
                 | "#SHA2" 
                 | #SHA2(Value)
```

## For `sha3_256` 

```k
                 | "#SHA3" 
                 | #SHA3(Value)
```

## For `blake2b_256` 

```k
                 | "#BLK2B" 
                 | #BLK2B(Value)
```                  

## For `verifySignature` 

```k
                 | "#VSIG"
                 | #VSIG(Value)
                 | #VSIG(Value, Value)
                 | #VSIG(Value, Value, Value)                 
```                  

## For `encodeUtf8`

```k
                 | "#EUTF" 
                 | #EUTF(Value)
```                 

## For `decodeUtf8`

```k
                 | "#DUTF" 
                 | #DUTF(Value)
```

## For `appendString`

```k
                 | "#ASTR" 
                 | #ASTR(Value)
                 | #ASTR(Value, Value)
```

## For `equalsString`

```k
                 | "#ESTR" 
                 | #ESTR(Value)
                 | #ESTR(Value, Value)
```

## For `chooseUnit`

```k
                 | "#CUT" 
                 | #CUT(Value)
```

```k 
endmodule
```

# UPLC Syntax

```k 
module UPLC-SYNTAX
  imports UPLC-CONCRETE-SYNTAX
  imports UPLC-ABSTRACT-SYNTAX
endmodule
```
