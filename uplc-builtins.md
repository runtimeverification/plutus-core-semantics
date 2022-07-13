# UPLC Builtins

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"
require "uplc-data-builtins.md"

module UPLC-BUILTINS
  imports UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-INTEGER-BUILTINS
  imports UPLC-BYTESTRING-BUILTINS
  imports UPLC-CRYPTO-BUILTINS
  imports UPLC-STRING-BUILTINS
  imports UPLC-DATA-BUILTINS

  rule <k> (builtin BN) => < builtin BN .List #numArgs(BN) > ... </k>
       <env> _ => .Map </env>
    requires notBool isPolyBuiltinName(BN)
     andBool notBool BN ==K chooseData

  rule <k> (builtin (_::PolyBuiltinName #Or chooseData) #as BN) ~> Force => < builtin BN .List #numArgs(BN) > ... </k>
       <env> _ => .Map </env>

  rule #typeCheck(ListItem(< con T _ >)          L, T:TypeConstant TS:TypeSignature) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(_:Value)              L, anyValue       TS:TypeSignature) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con list(_) _ >)    L, anyList        TS:TypeSignature) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con pair(_)(_) _ >) L, anyPair        TS:TypeSignature) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con T _ >)          L, mkConsCase     TS:TypeSignature) => #typeCheck(L, list(T))

  rule #typeCheck(.List, _) => true
  rule #typeCheck(    _, _) => false [owise]

endmodule
```
