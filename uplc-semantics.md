# UPLC semantics

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"
require "uplc-data-builtins.md"
require "uplc-hash.md"
require "uplc-discharge.md"

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
  imports UPLC-DISCHARGE

  syntax Bindable ::= Value

  syntax FinalState ::= "[]" Term
```

## Non-interactive application

```k
  syntax K ::= #app(Term, TermList, Map) [function]
  syntax K ::= #appAux(TermList, Map) [function]
  rule #app(M:Term, TL:TermList, RHO:Map) => M ~> #appAux(TL, RHO)
  rule #appAux(N:Term, RHO) => [_ N RHO ]
  rule #appAux(N:Term TL:TermList, RHO) => [_ N RHO ] ~> #appAux(TL, RHO) [owise]

```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:UplcId => #lookup(RHO, X, Heap) ... </k>
       <env> RHO </env>
       <heap> Heap </heap>
  requires #in(RHO, X)

  rule <k> X:UplcId => (error) ... </k>
       <env> RHO </env>
  requires notBool(#in(RHO, X))

  rule <k> (con T:TypeConstant C:Constant) =>
           < con T:TypeConstant C:Constant > ... </k>

  rule <k> (lam X:UplcId M:Term) => < lam X M RHO > ... </k>
       <env> RHO:Map </env>

  rule <k> (delay M:Term) => < delay M RHO > ... </k>
       <env> RHO:Map </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> < delay M:Term RHO:Map > ~> Force => M ... </k>
       <env> _ => RHO:Map </env>

  rule <k> [ M:Term TL:TermList ] => #app(M, TL, RHO) ... </k>
       <env> RHO:Map </env>

  rule <k> V:Value ~> [_ M RHO:Map ] => M ~> [ V _] ... </k>
       <env> _ => RHO </env>

  rule <k> V:Value ~> [ < lam X:UplcId M:Term RHO:Map > _] => M ... </k>
       <env> _ => #push( RHO, X, #uplcHash(V) ) </env>
       <heap> Heap => Heap[ #uplcHash(V) <- V ] </heap>

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List 1 > _] =>
           #eval(BN, (L ListItem(V))) ... </k>
  requires #typeCheck(L ListItem(V), BN, #numArgs(BN))

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List I:Int > _] =>
           < builtin BN (L ListItem(V)) (I -Int 1) > ... </k>
  requires I >Int 1 andBool #typeCheck(L ListItem(V), BN, #numArgs(BN) -Int I +Int 1)

  rule <k> V:Value ~> [ < builtin BN L I > _] ~> _ => (error) </k>
  requires notBool(#typeCheck(L ListItem(V), BN, #numArgs(BN) -Int I +Int 1))

  rule <k> _V:Value ~> [ < con _ _ > _] ~> _ => (error) </k>

  rule <k> _V:Value ~> [ < delay _ _ > _] ~> _ => (error) </k>

  rule <k> V:Value ~> . => [] discharge(V, Heap) </k>
       <heap> Heap:Map </heap>
```

```k
endmodule
```
