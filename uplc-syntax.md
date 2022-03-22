# The Concrete Syntax of Untyped Plutus

```k
require "domains.md"
require "uplc-bytestring.md"
require "uplc-environment.md"

module UPLC-ID

  syntax UplcId ::= r"[A-Za-z][A-Za-z0-9\\_\\']*" [prec(1), token]

endmodule

module UPLC-SYNTAX
  imports LIST
  imports STRING
  imports INT-SYNTAX
  imports UPLC-BYTESTRING
  imports UPLC-ENVIRONMENT
  imports UPLC-ID

  syntax Program ::= ConcreteProgram
                   | FlatProgram

  syntax ConcreteProgram ::= "(" "program" Version Term ")"

  syntax FlatProgram ::= ByteString

  syntax Version ::= r"[0-9]+.[0-9]+.[0-9]+" [token]

  syntax Term ::= UplcId
                | "(" "con" TypeConstant Constant ")"
                | "(" "builtin" BuiltinName ")"
                | "(" "lam" UplcId Term ")"
                | "[" Term TermList "]"
                | "(" "delay" Term ")"
                | "(" "force" Term ")"
                | "(" "error" ")"

  syntax Value ::= "<" "con" TypeConstant Constant ">"
                 | "<" "lam" UplcId Term Env ">"
                 | "<" "delay" Term Env ">"
                 | "<" "builtin" BuiltinName List Int ">"

  syntax TermList ::= NeList{Term, ""}

  syntax TypeConstant ::= "integer"
                        | "data"
                        | "bytestring"
                        | "string"
                        | "unit"
                        | "bool"
                        | "list" "(" TypeConstant ")"
                        | "pair" "(" TypeConstant ")" "(" TypeConstant ")"

  syntax Constant ::= Int
                    | "True"
                    | "False"
                    | ByteString
                    | String
                    | "()"
                    | "[" ConstantList "]"
                    | "(" Constant "," Constant ")"
                    | "{" TextualData "}"

  syntax ConstantList ::= List{Constant, ","}

  syntax TextualData ::= "Constr" Int "[" DataList "]"
                       | "Map" "[" DataPairList "]"
                       | "List" "[" DataList "]"
                       | "Integer" Int
                       | "ByteString" ByteString

  syntax DataList ::= List{TextualData, ","}
  syntax DataPair ::= "(" TextualData "," TextualData ")"
  syntax DataPairList ::= List{DataPair, ","}
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
