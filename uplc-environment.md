# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-ENVIRONMENT
  imports UPLC-ID
  imports BOOL
  imports K-EQUAL

  syntax Bindable
  syntax Bind ::= bind(UplcId, Bindable)
  syntax Env ::= List{Bind,""} 

  syntax Bool ::= #in(Env, UplcId) [function]
  rule #in(.Env, _) => false 
  rule #in(bind(X:UplcId, _) _, X) => true
  rule #in(bind(Y:UplcId, _) E:Env, X:UplcId) => #in(E, X)
  requires X =/=K Y
  
  syntax Bindable ::= #lookup(Env, UplcId) [function]
  rule #lookup(bind(X:UplcId, V:Bindable) _, X:UplcId) => V
  rule #lookup(bind(X:UplcId, _) E:Env, Y:UplcId) => #lookup(E, Y)
  requires X =/=K Y

  syntax Env ::= #push(Env, Bind) [function]
  rule #push(E:Env, B:Bind) => (B E)
endmodule
```