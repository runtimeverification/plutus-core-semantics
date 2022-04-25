# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-ENVIRONMENT
  imports UPLC-ID
  imports BOOL
  imports K-EQUAL
  imports MAP

  syntax Bindable
  syntax Bind ::= bind(UplcId, Int)

  syntax Env ::= List{Bind,""} 

  syntax Bool ::= #in(Env, UplcId) [function]
  rule #in(.Env, _) => false 
  rule #in(bind(X:UplcId, _) _, X) => true
  rule #in(bind(Y:UplcId, _) E:Env, X:UplcId) => #in(E, X)
  requires X =/=K Y
  
  syntax Value ::= #lookup(Env, UplcId, Map) [function]
  rule #lookup(bind(X:UplcId, I:Int) _, X:UplcId, M:Map) => {M[I]}:>Value
  rule #lookup(bind(X:UplcId, _) E:Env, Y:UplcId, M:Map) => #lookup(E, Y, M)
  requires X =/=K Y

  syntax Env ::= #push(Env, Bind) [function]
  rule #push(E:Env, B:Bind) => (B E)
endmodule
```