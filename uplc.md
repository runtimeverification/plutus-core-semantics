```k
require "uplc-syntax.md"
require "uplc-semantics.md"

module UPLC
  imports UPLC-SYNTAX
  imports UPLC-SEMANTICS
endmodule

module UPLC-WITHOUT-GENV
  imports UPLC

  rule #inKeysgEnv(_) => false
  rule gLookup(_) => < delay (error) .Map >
endmodule
```
