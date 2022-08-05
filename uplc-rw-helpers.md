# UPLC Global Environment

```k
require "uplc-syntax.md"

module UPLC-RW-HELPERS
  imports UPLC-SYNTAX

  // Error throwing with lambda
    syntax Term ::= "THROW_ERROR_LAM" [alias]
    rule THROW_ERROR_LAM =>
      (lam err_id
        (lam ds_0
          [
            (lam thunk_0 (error))
            [
              (force [ unit_match [ (force (builtin trace)) err_id unit_id ] ])
              ( con unit () )
            ]
          ]
        )
      )

    // Error throwing with delay
    syntax Term ::= "THROW_ERROR_DELAY" [alias]
    rule THROW_ERROR_DELAY =>
      (lam err_id
        (delay
          [
            (lam thunk_0 (error))
            [
              (force [ unit_match [ (force (builtin trace)) err_id unit_id ] ])
              ( con unit () )
            ]
          ]
        )
      )

endmodule
```
