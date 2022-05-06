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
  rule hexString2Int("0") => 0
  rule hexString2Int("1") => 1
  rule hexString2Int("2") => 2
  rule hexString2Int("3") => 3
  rule hexString2Int("4") => 4
  rule hexString2Int("5") => 5
  rule hexString2Int("6") => 6
  rule hexString2Int("7") => 7
  rule hexString2Int("8") => 8
  rule hexString2Int("9") => 9
  rule hexString2Int("A") => 10
  rule hexString2Int("B") => 11
  rule hexString2Int("C") => 12
  rule hexString2Int("D") => 13
  rule hexString2Int("E") => 14
  rule hexString2Int("F") => 15
  rule hexString2Int("a") => 10
  rule hexString2Int("b") => 11
  rule hexString2Int("c") => 12
  rule hexString2Int("d") => 13
  rule hexString2Int("e") => 14
  rule hexString2Int("f") => 15
  rule hexString2Int(S) =>
       (hexString2Int(head(S)) *Int (16 ^Int (lengthString(S) -Int 1)) +Int hexString2Int(tail(S)))
  requires lengthString(S) >Int 1
  
  syntax String ::= int2HexStringBasic(Int) [function]
  rule int2HexStringBasic(0) => "0"
  rule int2HexStringBasic(1) => "1"
  rule int2HexStringBasic(2) => "2"
  rule int2HexStringBasic(3) => "3"
  rule int2HexStringBasic(4) => "4"
  rule int2HexStringBasic(5) => "5"
  rule int2HexStringBasic(6) => "6"
  rule int2HexStringBasic(7) => "7"
  rule int2HexStringBasic(8) => "8"
  rule int2HexStringBasic(9) => "9"
  rule int2HexStringBasic(10) => "a"
  rule int2HexStringBasic(11) => "b"
  rule int2HexStringBasic(12) => "c"
  rule int2HexStringBasic(13) => "d"
  rule int2HexStringBasic(14) => "e"
  rule int2HexStringBasic(15) => "f"

  syntax String ::= int2HexString(Int) [function]
  rule int2HexString(I) => int2HexStringAux(I modInt 256 , "")

  syntax String ::= int2HexStringAux(Int, String) [function]   
  rule int2HexStringAux(0, "") => "0"

  rule int2HexStringAux(0, S) => S
  requires S =/=String ""

  rule int2HexStringAux(I, S) =>
       int2HexStringAux((I divInt 16), int2HexStringBasic(I modInt 16) +String S) [owise]

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

