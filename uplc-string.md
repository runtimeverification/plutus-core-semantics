# UPLC String

This module implements functions for string handling in UPLC.

```k
requires "domains.md"
requires "uplc-bytestring.md"

module UPLC-STRING
  imports UPLC-BYTESTRING
  imports STRING
  imports STRING-BUFFER
```

## `#decodeUtf8`

```k
  syntax String ::= #decodeUtf8(ByteString) [function]
  rule #decodeUtf8(B) => decodeUtf8Aux(trimByteString(B), "")

  syntax String ::= decodeUtf8Aux(String, String) [function]
  rule decodeUtf8Aux("", S) => S
  rule decodeUtf8Aux(S1, S2) =>
       decodeUtf8Aux(tail(tail(S1)), S2 +String
                     hexString2Char(head(S1) +String head(tail(S1))))
  requires lengthString(S1) >=Int 2
```

## `#encodeUtf8`

```k
  syntax ByteString ::= #encodeUtf8(String) [function]
  rule #encodeUtf8(S) => unTrimByteString(encodeUtf8Aux(S, ""))

  syntax String ::= encodeUtf8Aux(String, String) [function]
  rule encodeUtf8Aux("", S) => S
  rule encodeUtf8Aux(S1, S2) =>
       encodeUtf8Aux(tail(S1), S2 +String char2HexString(head(S1))) [owise]
```

## `#appendString`

```k
  syntax String ::= #appendString(String, String) [function]
  rule #appendString(S1, S2) => S1 +String S2
```

## `#equalString`

```k
  syntax Bool ::= #equalsString(String, String) [function]
  rule #equalsString(S1, S2) => S1 ==String S2
```

```k
endmodule
```