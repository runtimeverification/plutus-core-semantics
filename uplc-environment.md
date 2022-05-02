# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-ENVIRONMENT
  imports UPLC-ID
  imports BOOL-SYNTAX
  imports INT-SYNTAX
  imports MAP
  imports LIST

  syntax Bool ::= #in(Map, UplcId) [function, functional]
  rule #in(E:Map, X:UplcId) => X in_keys(E)
  
  syntax Value ::= #lookup(Map, UplcId, Map) [function]
  rule #lookup((_ X:UplcId |-> _ ListItem(I:Int)), X, H:Map) => {H[I]}:>Value

  syntax Map ::= #push(Map, UplcId, Int) [function, functional]
  rule #push(E:Map, X:UplcId, I:Int) => E[X <- ({E[X]}:>List ListItem(I))]
  requires X in_keys(E)
  rule #push(E:Map, X:UplcId, I:Int) => E[ X <- ListItem(I)] [owise]
endmodule
```