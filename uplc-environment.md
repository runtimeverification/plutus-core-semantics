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
  imports UPLC-SYNTAX
  imports LIST
  imports K-EQUAL

  syntax Value ::= #lookup(Map, UplcId) [function]
  rule #lookup(E:Map,  X:UplcId) => { E[X] }:>Value

  syntax Map ::= #push(Map, UplcId, Value) [function]
  rule #push(E:Map, X:UplcId, V:Value) => E [X <- V]

endmodule
```
