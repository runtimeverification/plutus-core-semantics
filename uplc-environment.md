# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-ENVIRONMENT
  imports BOOL-SYNTAX
  imports MAP
  imports LIST
  imports BOOL
  imports INT-SYNTAX
  imports K-EQUAL

  syntax KItem ::= #last(List) [function, functional]
  rule #last(L:List) => L[-1]

  syntax Value ::= #lookup(Map, KItem, Map) [function]
  rule #lookup((_ X:KItem |-> _ ListItem(I:KItem)), X,
               (_ I:KItem |-> _ ListItem(V:Value))) => V

  syntax Map ::= #push(Map, KItem, KItem) [function, functional]
  rule #push(E:Map, X:KItem, I:KItem) => E[X <- ({E[X]}:>List ListItem(I))]
  requires X in_keys(E) andBool #last({E[X]}:>List) =/=K I

  rule #push(E:Map, X:KItem, I:KItem) => E
  requires X in_keys(E) andBool notBool(#last({E[X]}:>List) =/=K I)

  rule #push(E:Map, X:KItem, I:KItem) => E[X <- ListItem(I)] [owise]
endmodule
```