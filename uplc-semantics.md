# UPLC semantics

```k
require "uplc-builtins.md"
require "uplc-discharge.md"

module UPLC-SEMANTICS
  imports INT
  imports MAP
  imports UPLC-BUILTINS
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

  rule <k> X:UplcId => #lookup(RHO, X) ... </k>
       <env> RHO => .Map </env>
  requires X in_keys(RHO)

  rule <k> X:UplcId => (error) ... </k>
       <env> RHO </env>
  requires notBool(X in_keys(RHO))

  rule <k> (con T:TypeConstant C:Constant) => < con T:TypeConstant C:Constant > ... </k>
       <env> _ => .Map </env>

  rule <k> (builtin BN) => < builtin BN .List | #expectedArguments(BN) > ... </k>
       <env> _ => .Map </env>

  rule <k> (lam X:UplcId M:Term) => < lam X M RHO > ... </k>
       <env> RHO => .Map </env>

  rule <k> (delay M:Term) => < delay M RHO > ... </k>
       <env> RHO => .Map </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> < delay M:Term RHO:Map > ~> Force => M ... </k>
       <env> _ => RHO </env>

  rule <k> [ M:Term TL:TermList ] => #app(M, TL, RHO) ... </k>
       <env> RHO </env>

  rule <k> V:Value ~> [_ M RHO:Map ] => M ~> [ V _] ... </k>
       <env> _ => RHO </env>

  rule <k> V:Value ~> [ < lam X:UplcId M:Term RHO:Map > _] => M ... </k>
       <env> _ => #push( RHO, X, V ) </env>

  rule <k> < builtin BN:BuiltinName L | ListItem(_:Quantification) E > ~> Force =>
           < builtin BN L | E > ... </k>
  requires notBool E ==K .List

  rule <k> < builtin BN:BuiltinName L | ListItem(_:Quantification)   > ~> Force =>
           #eval(BN, L) ... </k>

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List | ListItem(I:TypeVariable) > _] =>
           #eval(BN, (L ListItem(V))) ... </k>
  requires V ~ I

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List | ListItem(I:TypeVariable) E > _] =>
           < builtin BN (L ListItem(V)) | E > ... </k>
  requires V ~ I
   andBool E =/=K .List

  rule <k> _:Value ~> [ < builtin _ _ | _ > _] ~> _ => (error) </k> [owise]

  rule <k> _V:Value ~> [ < con _ _ > _] ~> _ => (error) </k>

  rule <k> _V:Value ~> [ < delay _ _ > _] ~> _ => (error) </k>

  rule <k> V:Value ~> . => [] discharge(V) </k>
```

```k
endmodule
```
