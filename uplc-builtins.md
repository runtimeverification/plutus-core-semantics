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

  syntax Bool ::= Value        "~" TypeVariable [function, klabel(typeCompatible),    symbol]
                | TypeConstant "~" TypeVariable [function, klabel(typeCompatibleAux), symbol]
  rule < con A _ > ~ X                       => A ~ X
  rule _:Value     ~ _:FullyPolyTypeVariable => true
  rule _:Value     ~ _                       => false [owise]

  rule A:TypeConstant ~ A                         => true
  rule list(A)        ~ listTV(X)                 => A ~ X
  rule pair(A)(B)     ~ pairTV(X, Y)              => A ~ X andBool B ~ Y
  rule _:TypeConstant ~ _:PolyBuiltinTypeVariable => true
  rule _:TypeConstant ~ _:FullyPolyTypeVariable   => true
  rule _:TypeConstant ~ _                         => false [owise]

  rule #typeCheck(ListItem(< con T _ >)          L, ListItem(T:TypeConstant) TS:List) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(_:Value)              L, ListItem(anyValue)       TS:List) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con list(_) _ >)    L, ListItem(anyList)        TS:List) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con pair(_)(_) _ >) L, ListItem(anyPair)        TS:List) => #typeCheck(L, TS)
  rule #typeCheck(ListItem(< con T _ >)          L, ListItem(mkConsCase)    _TS:List) => #typeCheck(L, ListItem(list(T)))

  rule #typeCheck(.List, _) => true
  rule #typeCheck(    _, _) => false [owise]

endmodule
```
