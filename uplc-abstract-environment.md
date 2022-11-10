# UPLC Abstract Environment

```k
require "uplc-syntax.md"

module UPLC-ABSTRACT-ENVIRONMENT
  imports BOOL
  imports MAP
  imports UPLC-SYNTAX

  syntax Value ::= #lookup(Map, UplcId)      [function]
  syntax Map   ::= #push(Map, UplcId, Value) [function, total]
  syntax Bool  ::= #def(Map, UplcId)         [function, total]

  rule #push(E:Map, X:UplcId, V:Value) => E [X <- V]
endmodule
```
