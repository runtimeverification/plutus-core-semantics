# UPLC Hash

Caveat: this function is a potential bottleneck. A faster hashing
scheme will be loocked at, because serializing to Kore is an extra
process call, and calling `Sha3_256` also takes time. The LLVM
backend already natively is computing hashes of things, and can do
that directly if that functionality can be exposed.
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