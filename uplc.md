```k
require "uplc-syntax.md"
require "uplc-semantics.md"
require "uplc-environment.md"

module UPLC-WITH-LOCAL-ENV
  imports UPLC-SYNTAX
  imports UPLC-SEMANTICS
  imports UPLC-ENVIRONMENT-LOCAL
endmodule

module UPLC-WITH-LOCAL-GLOBAL-ENV
  imports UPLC-SYNTAX
  imports UPLC-SEMANTICS
  imports UPLC-ENVIRONMENT-LOCAL-GLOBAL

  rule #inKeysgEnv(_) => false [owise]
endmodule
```
