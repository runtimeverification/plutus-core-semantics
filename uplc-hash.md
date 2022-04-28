# UPLC Hash

```k
requires "domains.md"
requires "uplc-syntax.md"
requires "krypto.md"

module UPLC-HASH
  imports INT
  imports STRING
  imports KRYPTO
  imports K-REFLECTION
  imports UPLC-SYNTAX

  syntax Int ::= #uplcHash(Value) [function]
  rule #uplcHash(V:Value) => String2Base(Sha3_256(#unparseKORE(V)), 16)
endmodule
```