# UPLC semantics

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"
require "uplc-data-builtins.md"
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
  imports UPLC-DISCHARGE

  syntax Bindable ::= Value

  syntax FinalState ::= "[]" Term
```

## Non-interactive application

```k
  syntax K ::= #app(Term, TermList, Int) [function]
  syntax K ::= #appAux(TermList, Int) [function]
  rule #app(M:Term, TL:TermList, ID:Int) => M ~> #appAux(TL, ID)
  rule #appAux(N:Term, ID) => [_ N ID ]
  rule #appAux(N:Term TL:TermList, ID) => [_ N ID ] ~> #appAux(TL, ID) [owise]

```

## CEK machine

```k
  rule <k> (program _V M) => M </k>

  rule <k> X:UplcId => #lookup(RHO, X) ... </k>
       <currentEnv> ID => 0 </currentEnv>
       <envID>    ID  </envID>
       <mappings> RHO </mappings>
  requires X in_keys(RHO)

  rule <k> X:UplcId => (error) ... </k>
       <currentEnv> ID </currentEnv>
       <envID>    ID  </envID>
       <mappings> RHO </mappings>
  requires notBool(X in_keys(RHO))

  rule <k> (con T:TypeConstant C:Constant) => < con T:TypeConstant C:Constant > ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> (lam X:UplcId M:Term) => < lam X M ID > ... </k>
       <currentEnv> ID => 0 </currentEnv>

  rule <k> (delay M:Term) => < delay M ID > ... </k>
       <currentEnv> ID => 0 </currentEnv>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> < delay M:Term ID:Int > ~> Force => M ... </k>
       <currentEnv> _ => ID </currentEnv>

  rule <k> [ M:Term TL:TermList ] => #app(M, TL, ID) ... </k>
       <currentEnv> ID </currentEnv>

  rule <k> V:Value ~> [_ M ID:Int ] => M ~> [ V _] ... </k>
       <currentEnv> _ => ID </currentEnv>

  rule <k> V:Value ~> [ < lam X:UplcId M:Term ID > _] => M ... </k>
       <currentEnv> _ => !ID </currentEnv>
       <envs>
       <env>
         <envID>    ID  </envID>
         <mappings> RHO </mappings>
       </env>
       ( .Bag => <env> <envID> !ID </envID> <mappings> #push( RHO, X, V ) </mappings> </env> )
       ...
       </envs>

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

  rule <k> V:Value ~> . => [] discharge(V) </k>
```

```k
endmodule
```
