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


  syntax K ::= #app(Term, TermList) [function]

  rule #app(M, .TermList) => M
  rule #app(M, (N:Term T:TermList)) => #app(M, T) ~> [_ N] [owise] 
```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:Id => V ... </k>
       <env> (_RHO:Map X |-> Clos(V, RHO')) => RHO' </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> [ M T ] => #app(M, T) ... </k> [owise]

  rule <k> V:Value ~> [_ N] => N ~> [ Clos(V, RHO) _] ... </k>
       <env> RHO </env>

  rule <k> (V:Value ~> ([ Clos((lam X:Id M:Term), RHO') _] )) => M ... </k>
       <env> RHO => (RHO' (X |-> Clos(V, RHO))) </env>

  rule <k> (delay M:Term) ~> Force => M ... </k>
```

```k
endmodule
```
