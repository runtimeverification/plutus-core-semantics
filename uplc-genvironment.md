# UPLC Global Environment

```k
require "domains.md"
require "uplc-syntax.md"
require "uplc-environment.md"

module UPLC-GENVIRONMENT
  imports UPLC-ID

  // Presence in the global environment
  syntax Bool ::= #inKeysgEnv(UplcId) [function, total]

  // Lookup in the global environment
  syntax Value ::= gLookup(UplcId) [function, total]

endmodule
```
