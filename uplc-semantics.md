# UPLC semantics

```k
require "uplc-builtins.md"
require "uplc-discharge.md"
require "uplc-abstract-environment.md"
require "uplc-free-variables.md"
```

```k
module UPLC-SEMANTICS
  imports INT
  imports MAP
  imports SET
  imports UPLC-BUILTINS
  imports UPLC-DISCHARGE
  imports UPLC-FREE-VARIABLES
  imports UPLC-ABSTRACT-ENVIRONMENT

  syntax Bindable ::= Value

  syntax FinalState ::= "[]" Term
```

## Non-interactive application

```k
  syntax K ::= #app(Term, TermList, Map) [function, functional]
  syntax K ::= #appAux(TermList, Map) [function, functional]
  rule #app(M:Term, TL:TermList, RHO:Map) => M ~> #appAux(TL, RHO)
  rule #appAux(N:Term, RHO:Map) => [_ N cutEnv(RHO, N) ]
  rule #appAux(N:Term TL:TermList, RHO:Map) => [_ N cutEnv(RHO, N) ] ~> #appAux(TL, RHO)
```

## Environment cutting

```k
  syntax Map ::= cutEnv(Map, Term) [function, functional]
  rule cutEnv(RHO, T) => removeAll(RHO, keys(RHO) -Set #FV(T))
```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:UplcId => #lookup(RHO, X) ... </k>
       <env> RHO => .Map </env>
  requires #def(RHO, X)

  rule <k> X:UplcId => (error) ... </k>
       <env> RHO </env>
  requires notBool(#def(RHO, X))

  rule <k> (con T:TypeConstant C:Constant) => < con T:TypeConstant C:Constant > ... </k>
       <env> _ => .Map </env>

  rule <k> (builtin BN) => < builtin BN .List | #expectedArguments(BN) > ... </k>
       <env> _ => .Map </env>

  rule <k> (lam X:UplcId M:Term) => < lam X M cutEnv(RHO, (lam X M)) > ... </k>
       <env> RHO => .Map </env>

  rule <k> (delay M:Term) => < delay M cutEnv(RHO, M) > ... </k>
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
