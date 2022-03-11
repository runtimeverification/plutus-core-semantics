# UPLC crypto builtins 

```k
require "uplc-configuration.md"
require "uplc-bytestring.md"
require "krypto.md"

module UPLC-CRYPTO-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-BYTESTRING
  imports KRYPTO
  imports STRING-BUFFER
  imports BYTES
  imports K-EQUAL
```

## `sha3_256`

UPLC bytestrings are almost what K's Sha3_256 needs. However, a proper string needs to
be built from the ground up. This is done with the following steps:

1. We convert the given ByteString to a string representing an hex
     number and then do its decimal representation.
     `String2Base(trimByteString(B), 16)`.

2. The integer from step 1. is converted into a Byte representation,
     with possible leading zeros preserved.
     `Int2Bytes(lengthString(trimByteString(B)) /Int 2,
     StringBase(...), BE)`, where the expression
     `lengthString(trimByteString(B)) /Int 2` preserves the leading
     zeros.

3. The Bytes resulting from step 2 are then translated into a string
     that can be consumed by `Sha3_256`.

4. The last step simply converts the string resulting from step 3 into
a ByteString.

## `sha3_256`

```k 
  rule <k> (builtin sha3_256) => #SHA3 ... </k>

  rule <k> (V:Value ~> ([ Clos(#SHA3, _RHO) _])) => #SHA3(V) ... </k>

  rule <k> #SHA3((con bytestring B:ByteString)) =>
           (con bytestring unTrimByteString(Sha3_256(encode(B)))) ... </k>
```

## `sha2_256`

The same steps of `sha3_256` are taken to produce the proper string argument for `Sha256`.

```k 
  rule <k> (builtin sha2_256) => #SHA2 ... </k>

  rule <k> (V:Value ~> ([ Clos(#SHA2, _RHO) _])) => #SHA2(V) ... </k>

  rule <k> #SHA2((con bytestring B:ByteString)) =>
           (con bytestring unTrimByteString(Sha256(encode(B)))) ... </k>
```

## `blake2b_256`

The same steps of `sha3_256` are taken to produce the proper string argument for `Blake2b256`.

```k
  rule <k> (builtin blake2b_256) => #BLK2B ... </k>

  rule <k> (V:Value ~> ([ Clos(#BLK2B, _RHO) _])) => #BLK2B(V) ... </k>

  rule <k> #BLK2B((con bytestring B:ByteString)) =>
           (con bytestring unTrimByteString(Blake2b256(encode(B)))) ... </k>
```

## `verifySignature`

```k
  rule <k> (builtin verifySignature) => #VSIG ... </k>

  rule <k> (V:Value ~> ([ Clos(#VSIG, _RHO) _])) => #VSIG(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#VSIG(V1:Value), _RHO) _])) => #VSIG(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#VSIG(V1:Value, V2:Value), _RHO) _])) =>
           #VSIG(V1, V2, V3) ... </k>

  rule <k> #VSIG((con bytestring K:ByteString),
                 (con bytestring M:ByteString),
                 (con bytestring S:ByteString)) => (con bool True) ...
       </k>
  requires ED25519VerifyMessage(encode(K), encode(M), encode(S))

  rule <k> #VSIG(_,_,_) => (con bool False) ... </k> [owise]
```

```k
endmodule
``` 