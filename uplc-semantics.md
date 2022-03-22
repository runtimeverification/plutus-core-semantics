# UPLC semantics

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"

module UPLC-SEMANTICS
  imports UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-INTEGER-BUILTINS
  imports UPLC-BYTESTRING-BUILTINS
  imports UPLC-CRYPTO-BUILTINS
  imports UPLC-STRING-BUILTINS
  imports INT

  syntax Bindable ::= Value

  syntax K ::= #app(Term, TermList, Env) [function]

  rule #app(M, .TermList, _RHO) => M
  rule #app(M, (N:Term T:TermList), RHO) => #app(M, T, RHO) ~> [_ N RHO ] [owise]
```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:UplcId => #lookup(RHO, X) ... </k>
       <env> RHO </env>

  rule <k> (con T:TypeConstant C:Constant) =>
           < con T:TypeConstant C:Constant > ... </k>

  rule <k> (lam X:UplcId M:Term) => < lam X M RHO > ... </k>
       <env> RHO:Env </env>

  rule <k> (delay M:Term) => < delay M RHO > ... </k>
       <env> RHO:Env </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> [ M T ] => #app(M, T, RHO) ... </k>
       <env> RHO:Env </env>

  rule <k> V:Value ~> [_ M RHO:Env ] => M ~> [ V _] ... </k>
       <env> _ => RHO </env>

  rule <k> V:Value ~> [ < lam X:UplcId M:Term RHO:Env > _] => M ... </k>
       <env> _ => #push(RHO, bind(X, V)) </env>

  rule <k> < delay M:Term RHO:Env > ~> Force => M ... </k>
       <env> _ => RHO:Env </env>

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List I:Int > _] =>
           < builtin BN (L ListItem(V)) (I -Int 1) > ... </k>
  requires I >Int 1

  rule <k> V:Value ~> [ < builtin BN:BuiltinName L:List 1 > _] =>
           #eval(BN, (L ListItem(V))) ... </k>
```

```k
endmodule
```
