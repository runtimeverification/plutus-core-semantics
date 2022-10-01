# UPLC Environment

```k
require "uplc-syntax.md"
require "uplc-abstract-environment.md"
require "uplc-genvironment.md"
require "uplc-free-variables.md"

module UPLC-MAP
  imports MAP
  imports MAP-SYMBOLIC
endmodule

module UPLC-ENVIRONMENT-LOCAL
  imports UPLC-MAP
  imports UPLC-SYNTAX
  imports UPLC-ABSTRACT-ENVIRONMENT
  imports UPLC-FREE-VARIABLES

  rule #lookup(E:Map, X:UplcId) => { E[X] }:>Value
  rule #def(E:Map, X:UplcId) => X in_keys(E)
  rule #FV( X:UplcId ) => SetItem(X)  
endmodule

module UPLC-ENVIRONMENT-LOCAL-GLOBAL
  imports BOOL
  imports UPLC-ID
  imports UPLC-MAP
  imports UPLC-SYNTAX
  imports UPLC-GENVIRONMENT
  imports UPLC-FREE-VARIABLES
  imports UPLC-ABSTRACT-ENVIRONMENT

  rule #lookup(_:Map, X:UplcId) => gLookup(X)
  requires #inKeysgEnv(X)
  // (?) andBool notBool(X in_keys(E))

  rule #lookup(E:Map, X:UplcId) => { E[X] }:>Value
  requires notBool(#inKeysgEnv(X)) andBool
           X in_keys(E) 

  rule #def(E:Map, X:UplcId) =>
       #inKeysgEnv(X) orElseBool (X in_keys(E))

  rule #FV( X:UplcId ) => SetItem(X)
  requires notBool(#inKeysgEnv(X))
endmodule
```
