# UPLC semantics

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"
require "uplc-data-builtins.md"
require "uplc-hash.md"

module UPLC-SEMANTICS
  imports INT
  imports MAP
  imports UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-INTEGER-BUILTINS
  imports UPLC-BYTESTRING-BUILTINS
  imports UPLC-CRYPTO-BUILTINS
  imports UPLC-STRING-BUILTINS
  imports UPLC-DATA-BUILTINS
  imports UPLC-HASH

  syntax Bindable ::= Value

  syntax FinalState ::= "[]" "(" "con" TypeConstant Constant ")"
                      | "[]" "(" "lam" UplcId Term ")"
                      | "[]" "(" "delay" Term ")"
```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:UplcId => #lookup(RHO, X, Heap) ... </k>
       <env> RHO </env>
       <heap> Heap </heap>

  rule <k> (con T:TypeConstant C:Constant) =>
           < con T:TypeConstant C:Constant > ... </k>

  rule <k> (lam X:UplcId M:Term) => < lam X M RHO > ... </k>
       <env> RHO:Env </env>

  rule <k> (delay M:Term) => < delay M RHO > ... </k>
       <env> RHO:Env </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> < delay M:Term RHO:Env > ~> Force => M ... </k>
       <env> _ => RHO:Env </env>

  rule <k> [ M N ] => M ~> [_ N RHO ] ... </k>
       <env> RHO:Env </env>

  rule <k> V:Value ~> [_ M RHO:Env ] => M ~> [ V _] ... </k>
       <env> _ => RHO </env>

  rule <k> V:Value ~> [ < lam X:UplcId M:Term RHO:Env > _] => M ... </k>
       <env> _ => #push( RHO, bind( X, #uplcHash(V) ) ) </env>
       <heap> Heap => Heap[  #uplcHash(V) <- V ] </heap>

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List 1 > _] =>
           #eval(BN, (L ListItem(V))) ... </k>

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List I:Int > _] =>
           < builtin BN (L ListItem(V)) (I -Int 1) > ... </k>
  requires I >Int 1

  rule <k> < con T:TypeConstant C:Constant > ~> . => [] (con T C) </k>

  rule <k> < lam I:UplcId T:Term _E:Env > ~> . => [] (lam I T) </k>

  rule <k> < delay T:Term _E:Env > ~> . => [] (delay T) </k>

  rule <k> _V:Value ~> [ < con _ _ > _] => (error) ... </k>

  rule <k> _V:Value ~> [ < delay _ _ > _] => (error) ... </k>

  rule <k> < builtin _ _ _ > ~> . => (error) </k>
```

```k
endmodule
```
