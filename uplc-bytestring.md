# UPLC Bytestring datatype

```k
require "domains.md"

module UPLC-BYTESTRING-SYNTAX
  syntax ByteString ::= r"#([a-fA-F0-9][a-fA-F0-9])*" [token]
endmodule
```

## Hexadecimal and bytestring cohercions

```k
module UPLC-HEXADECIMAL
  imports UPLC-BYTESTRING-SYNTAX
  imports INT
  imports STRING
  imports STRING-BUFFER
  imports K-EQUAL

  syntax String ::= head(String) [function]
  rule head(S) => substrString(S, 0, 1)

  syntax String ::= tail(String) [function]   
  rule tail(S) => substrString(S, 1, lengthString(S))

  syntax Int ::= hexString2Int(String) [function]
  rule hexString2Int(S) => String2Base(S, 16)

  syntax String ::= int2HexString(Int) [function]
  rule int2HexString(I) => Base2String(I, 16)

  syntax String ::= hexString2Char(String) [function]
  rule hexString2Char(S) => chrChar(String2Base(S, 16))
  requires lengthString(S) ==Int 2

  syntax String ::= char2HexString(String) [function]
  rule char2HexString(S) => int2HexString(ordChar(S))
endmodule
```

## Bytestring helper functions

```k
module UPLC-BYTESTRING

  imports UPLC-BYTESTRING-SYNTAX
  imports UPLC-HEXADECIMAL
  imports STRING
  imports STRING-BUFFER
  imports INT
  imports K-EQUAL
  imports LIST
  imports BOOL
  imports BYTES

  syntax String ::= ByteString2String (ByteString) [function, functional, hook(STRING.token2string)]
  
  syntax ByteString ::= String2ByteString (String) [function, functional, hook(STRING.string2token)]

  syntax String ::= trimByteString(ByteString) [function]
  rule trimByteString(B) => substrString(ByteString2String(B), 1, lengthString(ByteString2String(B)))

  syntax ByteString ::= unTrimByteString(String) [function]
  rule unTrimByteString(S) => String2ByteString("#" +String S)

  syntax String ::= encode(ByteString) [function]
  rule encode(B) => ""
  requires trimByteString(B) ==String ""

  rule encode(B) =>
       Bytes2String(Int2Bytes(lengthString(trimByteString(B)) /Int 2,
                              String2Base(trimByteString(B), 16 ), BE)) [owise]

  syntax String ::= padZero(String) [function]
  rule padZero(S) => S
  requires (lengthString(S) modInt 2) ==Int 0

  rule padZero(S) => ("0" +String S) [owise]

  syntax List ::= mkHexStringList(String) [function]
  rule mkHexStringList(S) => mkHexStringListAux(S, .List)
  requires (lengthString(S) modInt 2) ==Int 0

  syntax List ::= mkHexStringListAux(String, List) [function]   
  rule mkHexStringListAux("", L) => L

  rule mkHexStringListAux(S, L) =>
       mkHexStringListAux(tail(tail(S)), L ListItem(head(S) +String head(tail(S)))) [owise]
       
  syntax String ::= mkHexString(List) [function]
  rule mkHexString(L) => mkHexStringAux(L, "")

  syntax String ::= mkHexStringAux(List, String) [function]   
  rule mkHexStringAux(.List, S) => S

  rule mkHexStringAux(ListItem(S1) L, S2) =>
       mkHexStringAux(L, S2 +String  S1)

  syntax ByteString ::= #appendByteString(ByteString, ByteString) [function]
  rule #appendByteString(B1, B2) =>
       String2ByteString("#" +String trimByteString(B1) +String trimByteString(B2))

  syntax Int ::= #lengthOfByteString(ByteString) [function]
  rule #lengthOfByteString(B) =>
       lengthString(trimByteString(B)) divInt 2

  syntax ByteString::= #consByteString(Int, ByteString) [function]
  rule #consByteString(I, B) =>
       String2ByteString("#" +String padZero(int2HexString(I)) +String trimByteString(B))

  syntax String ::= sliceByteStringList(Int, Int, List) [function]
  rule sliceByteStringList(I, J, _L) => ""
  requires J <Int I

  rule sliceByteStringList(I, J, L) => mkHexString(range(L, I, size(L) -Int 1 -Int J)) [owise]

  syntax ByteString ::= #sliceByteString(Int, Int, ByteString) [function]
  rule #sliceByteString(S, K, B) =>
       String2ByteString("#" +String
        sliceByteStringList(maxInt(S, 0),
                            minInt(S +Int K -Int 1, size(mkHexStringList(trimByteString(B))) -Int 1),
                             mkHexStringList(trimByteString(B))))

  syntax String ::= getHexStringFromHexList(List, Int) [function]
  rule getHexStringFromHexList(L, I) => {L[I]}:>String 

  syntax Int ::= #indexByteString(ByteString, Int) [function]
  rule #indexByteString(B, I) =>
       hexString2Int(getHexStringFromHexList(mkHexStringList(trimByteString(B)), I))

  syntax Bool ::= #equalsByteString(ByteString, ByteString) [function]
  rule #equalsByteString(B1, B2) =>
       hexString2Int(trimByteString(B1)) ==Int hexString2Int(trimByteString(B2))

  syntax Bool ::= #lessThanByteString(ByteString, ByteString) [function]
  rule #lessThanByteString(B1, B2) =>
       hexString2Int(trimByteString(B1)) <Int hexString2Int(trimByteString(B2))

  syntax Bool ::= #lessThanEqualsByteString(ByteString, ByteString) [function]
  rule #lessThanEqualsByteString(B1, B2) =>
       hexString2Int(trimByteString(B1)) <=Int hexString2Int(trimByteString(B2))
endmodule
```

