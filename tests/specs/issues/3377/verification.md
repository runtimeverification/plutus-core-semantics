# DJEC Oracle Contract Verification

```k
requires "domains.md"
requires "verification-common.md"
requires "uplc-genvironment-instance.md"
```

```k
module LIST-AUXILIARIES
  imports UPLC-SYNTAX

  syntax Bool ::= inList(Constant, ConstantList) [function, total]

  rule inList(_, .ConstantList) => false [simplification]
  rule inList(C:Constant, CH:Constant, _:ConstantList) => true
       requires C ==K CH [simplification]

  rule inList(C:Constant, CH:Constant, CL:ConstantList) => inList(C, CL)
       requires notBool C ==K CH [simplification]
endmodule
```

```k
module ABSTRACTION-AUXILIARIES
  imports UPLC-SYNTAX
  imports UPLC-DATA-BUILTINS
  imports UPLC-GENVIRONMENT-INSTANCE
```

The `CustomTxInInfoListHashes(CL)` function returns the list of script
credential hashes from the list of transactions `CL`.

```k
  syntax ConstantList ::= CustomTxInInfoListHashes(ConstantList) [function]

  rule CustomTxInInfoListHashes(.ConstantList) => .ConstantList

  rule CustomTxInInfoListHashes([ [ _:ConstantList ],
                                  [ [ [ 0, _:ByteString ], [ _:Int, _:ConstantList ] ],
                                    { _:TextualData },
                                    { _:TextualData } ] ], CL:ConstantList) => CustomTxInInfoListHashes(CL) [simplification]

  rule CustomTxInInfoListHashes([ [ _:ConstantList ],
                                  [ [ [ 1, SH:ByteString ], [ _:Int, _:ConstantList ] ],
                                    { _:TextualData },
                                    { _:TextualData } ] ], CL:ConstantList) => SH, CustomTxInInfoListHashes(CL) [simplification]

  rule #Ceil(CustomTxInInfoListHashes(CL:ConstantList)) => #Ceil(DList("CustomTxInInfo", CL)) [simplification]
```

```k
endmodule
```

### Proof tactics

```k
module PROOF-TACTICS
  imports UPLC-SYNTAX

  syntax Bool ::= DestructConstantList(ConstantList) [function, no-evaluators]

  rule { true #Equals DestructConstantList(CL:ConstantList) } =>
    { CL #Equals .ConstantList } #Or
    (#Exists DLCH:Constant. #Exists DLCT:ConstantList. { CL #Equals DLCH, DLCT })
    [simplification]

endmodule
```

## Main module

```k
module VERIFICATION
  imports VERIFICATION-COMMON
  imports LIST-AUXILIARIES
  imports ABSTRACTION-AUXILIARIES
  imports PROOF-TACTICS

endmodule
```
