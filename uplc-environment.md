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

  syntax Int ::= #last(List) [function]
  rule #last(L:List) => {L[-1]}:>Int

  syntax Value ::= #lookup(Map, UplcId) [function]

  // We could have a (spurious) case for when V is not a Value instead
  // of that coercion, but perhaps that is handled by the new Env type?
  rule [lookup.some]: #lookup(  E:Map,  X:UplcId) => { E[X] }:>Value
  rule [lookup.none]: #lookup( _E:Map, _X:UplcId) => { < con integer 0 > }:>Value [owise]

  syntax Map ::= #push(Map, UplcId, Value) [function]
  rule #push(E:Map, X:UplcId, V:Value) => E [X <- V]

endmodule
```
