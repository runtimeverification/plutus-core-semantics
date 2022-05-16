# UPLC crypto builtins 

```k
require "uplc-configuration.md"
require "uplc-bytestring.md"
require "krypto.md"

module KRYPTO-WRAPPERS-CONCRETE [concrete]
  imports KRYPTO
  
  syntax Bool ::= ED25519VerifyMessageWrapper(String, String, String) [function]
  rule ED25519VerifyMessageWrapper(S1:String, S2:String, S3:String) => ED25519VerifyMessage(S1, S2, S3)

  syntax String ::= "Sha3_256Wrapper" "(" String ")" [function]
  rule Sha3_256Wrapper(S:String) => Sha3_256(S)

  syntax String ::= Sha256Wrapper(String) [function]
  rule Sha256Wrapper(S:String) => Sha256(S)
  
  syntax String ::= Blake2b256Wrapper(String) [function]
  rule Blake2b256Wrapper(S:String) => Blake2b256(S)
endmodule

module KRYPTO-WRAPPERS-SYMBOLIC [symbolic]
  imports KRYPTO
  
  syntax Bool ::= ED25519VerifyMessageWrapper(String, String, String) [function, no-evaluators]
  syntax String ::= "Sha3_256Wrapper" "(" String ")" [function, no-evaluators]
  syntax String ::= Sha256Wrapper(String) [function, no-evaluators]
  syntax String ::= Blake2b256Wrapper(String) [function, no-evaluators]
endmodule

module KRYPTO-WRAPPERS
  imports KRYPTO-WRAPPERS-CONCRETE
  imports KRYPTO-WRAPPERS-SYMBOLIC
endmodule


module UPLC-CRYPTO-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-BYTESTRING
  imports KRYPTO-WRAPPERS
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
  rule <k> (builtin sha3_256) => < builtin sha3_256 .List 1 >  ... </k>

  rule <k> #eval(sha3_256, ListItem(< con bytestring B:ByteString >)) =>
           < con bytestring unTrimByteString(Sha3_256Wrapper(encode(B))) > ... </k>

  rule <k> #eval(sha3_256, _) ~> _ => (error) </k> [owise]
```

## `sha2_256`

The same steps of `sha3_256` are taken to produce the proper string argument for `Sha256`.

```k 
  rule <k> (builtin sha2_256) => < builtin sha2_256 .List 1 >  ... </k>

  rule <k> #eval(sha2_256, ListItem(< con bytestring B:ByteString >)) =>
           < con bytestring unTrimByteString(Sha256Wrapper(encode(B))) > ... </k>

  rule <k> #eval(sha2_256, _) ~> _ => (error) </k> [owise]
```

## `blake2b_256`

The same steps of `sha3_256` are taken to produce the proper string argument for `Blake2b256`.

```k
  rule <k> (builtin blake2b_256) => < builtin blake2b_256 .List 1 > ... </k>

  rule <k> #eval(blake2b_256, ListItem(< con bytestring B:ByteString >)) =>
           < con bytestring unTrimByteString(Blake2b256Wrapper(encode(B))) > ... </k>

  rule <k> #eval(blake2b_256, _) ~> _ => (error) </k> [owise]
```

## `verifySignature`

```k
  rule <k> (builtin verifySignature) => < builtin verifySignature .List 3 > ... </k>

  rule <k> #eval(verifySignature,
                 (ListItem(< con bytestring K:ByteString >)
                  ListItem(< con bytestring M:ByteString >)
                  ListItem(< con bytestring S:ByteString >))) =>
           < con bool True > ... </k>
  requires ED25519VerifyMessageWrapper(encode(K), encode(M), encode(S))

  rule <k> #eval(verifySignature,
                 (ListItem(< con bytestring K:ByteString >)
                  ListItem(< con bytestring M:ByteString >)
                  ListItem(< con bytestring S:ByteString >))) =>
           < con bool False > ... </k>
  requires notBool ED25519VerifyMessageWrapper(encode(K), encode(M), encode(S))

  rule <k> #eval(verifySignature, _) ~> _ => (error) </k> [owise]
```

```k
endmodule
``` 