# UPLC Environment

```k
require "domains.md"

module UPLC-ENVIRONMENT
  imports ID
  imports BOOL
  imports K-EQUAL

  syntax Bindable
  syntax Bind ::= bind(Id, Bindable)
  syntax Env ::= List{Bind,""} 

  syntax Bool ::= #in(Env, Id) [function]
  rule #in(.Env, _) => false 
  rule #in(bind(X:Id, _) _, X) => true
  rule #in(bind(Y:Id, _) E:Env, X:Id) => #in(E, X)
  requires X =/=K Y
  
  syntax Bindable ::= #lookup(Env, Id) [function]
  rule #lookup(bind(X:Id, V:Bindable) _, X:Id) => V
  rule #lookup(bind(X:Id, _) E:Env, Y:Id) => #lookup(E, Y)
  requires X =/=K Y

  syntax Env ::= #push(Env, Bind) [function]
  rule #push(E:Env, B:Bind) => (B E)
endmodule
```