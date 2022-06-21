# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-MAP
  imports MAP
  imports MAP-SYMBOLIC
endmodule

module UPLC-ENVIRONMENT
  imports UPLC-ID
  imports BOOL
  imports INT-SYNTAX
  imports UPLC-MAP
  imports LIST
  imports K-EQUAL

  syntax List ::= #append(List, Int) [function, functional]
  rule #append(L:List, I:Int) => L ListItem(I)

  syntax Int ::= #last(List) [function]
  rule #last(L:List) => {L[-1]}:>Int

  syntax Value ::= #lookup(Map, UplcId, Map) [function]
  rule #lookup(E:Map, X:UplcId, H:Map) => {H[#last({E[X]}:>List)]}:>Value
  requires X in_keys(E)

  syntax Map ::= #push(Map, UplcId, Int) [function]
  rule #push(E:Map, X:UplcId, I:Int) =>
       #if X in_keys(E)
       #then E[X <- #append({E[X] orDefault .List}:>List, I)]
       #else E[X <- ListItem(I)]
       #fi
endmodule
```
