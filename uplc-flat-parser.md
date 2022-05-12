# Flat Parser

```k
requires "uplc-syntax.md"
requires "bitstream.md"

module UPLC-FLAT-PARSER
  imports UPLC-SYNTAX
  imports BYTES
  imports BITSTREAM
  imports STRING
  imports DECODE-UTF8-BYTES
  imports DECODE-UTF8-BYTES-SYMBOLIC
  imports FLAT-STRING-HELPER
```

## Flat Parser Entrypoint

The following function is the entry point to the flat parser.

```k
  syntax ConcreteProgram ::= #bytes2program( Bytes )     [function]
                           | #bytes2program( String, K ) [function]

  rule #bytes2program( BYTES ) =>
    #let
      VERSION = #readVersion( BitStream( 0, BYTES ) )
    #in
      #bytes2program( #getDatum( VERSION ),
                      #readProgramTerm( #readTerm, BitStream( #getBitLength( VERSION ), BYTES ) )
                    )

  rule #bytes2program( VERSION, TERM:Term ~> . ) => ( program String2Version( VERSION ) TERM )
```

The following KItems are used to manage control in the function above.

```k
  syntax KItem ::= "#readTerm"
                 | "#readTermTag" Int
                 | "#readValue" Int
                 | "#readConType" Int
```

## Parsing The program version

The program version is specified as 3 unsigned integers. This implementation only accounts for
version numbers that are less than 7 bits and needs to be updated to parse larger integers.

```k
  syntax StringDatum ::= SDat( BitLength:Int, Datum:String )

  syntax Int ::= #getBitLength( StringDatum ) [function]
//------------------------------------------------------
  rule #getBitLength( SDat( I, _ ) ) => I

  syntax String ::= #getDatum( StringDatum ) [function]
//-----------------------------------------------------
  rule #getDatum( SDat( _, S ) ) => S

  syntax KItem ::= "#read1stVersion" String
                 | "#read2ndVersion" String
                 | "#read3rdVersion" String

  syntax StringDatum ::= #readVersion( BitStream )    [function]
  syntax StringDatum ::= #readVersion( K, BitStream ) [function]
//--------------------------------------------------------------
  rule #readVersion( BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      #readVersion( #read1stVersion #getDatumAsString( VARDATA ) +String ".",
                    BitStream( I +Int #getBitLength( VARDATA ), BYTES )
                  )

  rule #readVersion( #read1stVersion S, BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      #readVersion( #read2ndVersion S +String #getDatumAsString( VARDATA ) +String ".",
                    BitStream( I +Int #getBitLength( VARDATA ), BYTES )
                  )

  rule #readVersion( #read2ndVersion S, BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      SDat( I +Int #getBitLength( VARDATA ) , S +String #getDatumAsString( VARDATA ) )

  syntax String ::= #getDatumAsString( VarLenDatum ) [function]
//-------------------------------------------------------------
  rule #getDatumAsString( VDat( _ , D ) ) => Int2String( D )

```

## Reading Terms

```k
  syntax Term ::= #readProgramTerm( K, BitStream ) [function]

  rule #readProgramTerm( #readTerm => #readTermTag #readNBits( #termTagLength, BitStream( I, BYTES) ),
                         BitStream( I => I +Int #termTagLength, BYTES )
                       )

  rule #readProgramTerm( #readTermTag DELAY, BITSTREAM ) => ( delay #readProgramTerm( #readTerm, BITSTREAM ) )

  rule #readProgramTerm( #readTermTag ERROR, _ ) => ( error )

  rule #readProgramTerm( #readTermTag CON => #readConType #readType( BitStream( I, Bs ) ),
                         BitStream( I => I +Int #typeLength, Bs )
                       )

  rule #readProgramTerm( #readConType UNIT, _ ) => ( con unit () )

  rule #readProgramTerm( #readConType BOOL, BitStream( I, BYTES) ) => ( con bool #bit2boolval( #readNBits( 1, BitStream( I, BYTES ) ) ) )

  rule #readProgramTerm( #readConType INTEGER, BitStream( I, BYTES) ) => ( con integer #getDatum(#readIntegerValue( BitStream( I, BYTES ) ) ) )

  rule #readProgramTerm( #readConType STRING, BitStream( I, BYTES) ) => ( con string #readStringValue( BitStream( #nextByteBoundary(I), BYTES ) ) )

  rule #readProgramTerm( #readConType BYTESTRING, BitStream( I, BYTES) ) => ( con bytestring #readByteStringValue( BitStream( #nextByteBoundary(I), BYTES ) ) )

  syntax KItem ::= "#readBuiltinName" Int

  rule #readProgramTerm( #readTermTag BUILTIN => #readBuiltinName #readNBits( 8, BitStream( I, BYTES ) ),
                         BitStream( I => I +Int #builtinTagLength, BYTES )
                       )

  rule #readProgramTerm( #readBuiltinName Bn, _ ) => ( builtin #bn2BuiltinName( Bn ) )

  rule #readProgramTerm( TERM:Term ~> ., _ ) => TERM
```

### Utility Functions Used to Read Terms

```k
  syntax Constant ::= #bit2boolval(Int) [function]
//------------------------------------------------
  rule #bit2boolval(0) => False
  rule #bit2boolval(1) => True

  // TODO: fix this function to support parametric types

  syntax Int ::= #readType( BitStream ) [function]
//------------------------------------------------
  rule #readType( BitStream( I, BYTES ) ) => #readNBits( 4, BitStream( I +Int 1, BYTES ) )

  syntax Version ::= String2Version (String) [function, functional, hook(STRING.string2token)]
```

### Values used to represent Terms and Types

#### Values for term tags

The following `Int`s represent different terms terms and encoded using 4 bits.

```k
  syntax Int ::= "#termTagLength" [macro]
//---------------------------------------
  rule #termTagLength => 4

  syntax Int ::= "VAR"     [macro]
               | "DELAY"   [macro]
               | "LAMBDA"  [macro]
               | "APP"     [macro]
               | "CON"     [macro]
               | "FORCE"   [macro]
               | "ERROR"   [macro]
               | "BUILTIN" [macro]
//--------------------------------
  rule VAR     => 0
  rule DELAY   => 1
  rule LAMBDA  => 2
  rule APP     => 3
  rule CON     => 4
  rule FORCE   => 5
  rule ERROR   => 6
  rule BUILTIN => 7
```

#### Values for type

The following `Int`s represent types within the untypted plutus language and encoded in 4 bits.
They are used as parameters that describe constants or as parameters to builtin functions.

```k
  syntax Int ::= "#typeLength" [macro]
//------------------------------------
  rule #typeLength => 6

  syntax Int ::= "INTEGER"    [macro]
               | "BYTESTRING" [macro]
               | "STRING"     [macro]
               | "UNIT"       [macro]
               | "BOOL"       [macro]
               | "LIST"       [macro]
               | "PAIR"       [macro]
               | "TYPE_APP"   [macro]
               | "DATA"       [macro]
//-----------------------------------
  rule INTEGER    => 0
  rule BYTESTRING => 1
  rule STRING     => 2
  rule UNIT       => 3
  rule BOOL       => 4
  rule LIST       => 5
  rule PAIR       => 6
  rule TYPE_APP   => 7
  rule DATA       => 8
```

#### Values for Builtin Names

Tags for builtins use 8 bits allowing for a max of 256 builtin functions.

```k
  syntax Int ::= "#builtinTagLength" [macro]
//------------------------------------------
  rule #builtinTagLength => 8

  syntax BuiltinName ::= #bn2BuiltinName( Int ) [function]
//--------------------------------------------------------
  rule #bn2BuiltinName( 0 ) => addInteger
  rule #bn2BuiltinName( 1 ) => subtractInteger
  rule #bn2BuiltinName( 2 ) => multiplyInteger
  rule #bn2BuiltinName( 3 ) => divideInteger
  rule #bn2BuiltinName( 4 ) => quotientInteger
  rule #bn2BuiltinName( 5 ) => remainderInteger
  rule #bn2BuiltinName( 6 ) => modInteger
  rule #bn2BuiltinName( 7 ) => equalsInteger
  rule #bn2BuiltinName( 8 ) => lessThanInteger
  rule #bn2BuiltinName( 9 ) => lessThanEqualsInteger
  rule #bn2BuiltinName( 10 ) => appendByteString
  rule #bn2BuiltinName( 11 ) => consByteString
  rule #bn2BuiltinName( 12 ) => sliceByteString
  rule #bn2BuiltinName( 13 ) => lengthOfByteString
  rule #bn2BuiltinName( 14 ) => indexByteString
  rule #bn2BuiltinName( 15 ) => equalsByteString
  rule #bn2BuiltinName( 16 ) => lessThanByteString
  rule #bn2BuiltinName( 17 ) => lessThanEqualsByteString
  rule #bn2BuiltinName( 18 ) => sha2_256
  rule #bn2BuiltinName( 19 ) => sha3_256
  rule #bn2BuiltinName( 20 ) => blake2b_256
  rule #bn2BuiltinName( 21 ) => verifySignature
  rule #bn2BuiltinName( 22 ) => appendString
  rule #bn2BuiltinName( 23 ) => equalsString
  rule #bn2BuiltinName( 24 ) => encodeUtf8
  rule #bn2BuiltinName( 25 ) => decodeUtf8
  rule #bn2BuiltinName( 26 ) => ifThenElse
  rule #bn2BuiltinName( 27 ) => chooseUnit
  rule #bn2BuiltinName( 28 ) => trace
  rule #bn2BuiltinName( 29 ) => fstPair
  rule #bn2BuiltinName( 30 ) => sndPair
  rule #bn2BuiltinName( 31 ) => chooseList
  rule #bn2BuiltinName( 32 ) => mkCons
  rule #bn2BuiltinName( 33 ) => headList
  rule #bn2BuiltinName( 34 ) => tailList
  rule #bn2BuiltinName( 35 ) => nullList
  rule #bn2BuiltinName( 36 ) => chooseData
  rule #bn2BuiltinName( 37 ) => constrData
  rule #bn2BuiltinName( 38 ) => mapData
  rule #bn2BuiltinName( 39 ) => listData
  rule #bn2BuiltinName( 40 ) => iData
  rule #bn2BuiltinName( 41 ) => bData
  rule #bn2BuiltinName( 42 ) => unConstrData
  rule #bn2BuiltinName( 43 ) => unMapData
  rule #bn2BuiltinName( 44 ) => unListData
  rule #bn2BuiltinName( 45 ) => unIData
  rule #bn2BuiltinName( 46 ) => unBData
  rule #bn2BuiltinName( 47 ) => equalsData
  rule #bn2BuiltinName( 48 ) => mkPairData
  rule #bn2BuiltinName( 49 ) => mkNilData
  rule #bn2BuiltinName( 50 ) => mkNilPairData
```

### Variable Length Data

There are two parts to variable length data that are necessary for parsing: the actual datum and
the length of the datum. To facilitate parsing, the `VarLenDatum` sort has a datum that was parsed
and the bit length that was traversed when parsing the datum.

```k
  syntax VarLenDatum ::= VDat( BitLength:Int, Datum:Int )

  syntax Int ::= #getDatum( VarLenDatum ) [function]
//--------------------------------------------------
  rule #getDatum( VDat( _, D ) ) => D

  syntax Int ::= #getBitLength( VarLenDatum ) [function]
//------------------------------------------------------
  rule #getBitLength( VDat( BitLen, _ ) ) => BitLen

  syntax VarLenDatum ::= #getVarLenData( BitStream ) [function]
//-------------------------------------------------------------
  rule #getVarLenData( BITSTREAM ) => #getVarLenData( 0, 0, BITSTREAM )

  syntax VarLenDatum ::= #getVarLenData( CurrentByteOffset:Int, CurrentDatum:Int, BitStream ) [function]
//------------------------------------------------------------------------------------------------------
  rule #getVarLenData( I0 => I0 +Int 1,
                       D  => D +Int ( #readNBits( 7, BitStream( I1 +Int 1 , BYTES ) ) <<Int ( 7 *Int I0 ) ),
                       BitStream( I1, BYTES ) => BitStream( I1 +Int 8, BYTES )
                     )
    requires #readNBits( 1, BitStream( I1, BYTES ) ) ==Int 1

  rule #getVarLenData( I0, D, BitStream( I1, BYTES) ) =>
    VDat( ( I0 +Int 1 ) *Int 8, D +Int ( #readNBits( 7, BitStream( I1 +Int 1, BYTES ) ) <<Int ( 7 *Int I0 ) ) ) [owise]

```

### Reading Integer Values

```k
  syntax VarLenDatum ::= #readIntegerValue( BitStream ) [function]
//----------------------------------------------------------------
  rule #readIntegerValue( Bs ) => #readIntegerValue( #getVarLenData( Bs ) )

  syntax VarLenDatum ::= #readIntegerValue( VarLenDatum ) [function]
//------------------------------------------------------------------
  rule #readIntegerValue( VDat( Len, Dat ) ) => VDat( Len, #decodeZigZag( Dat ) )
```

This zigzag decoding algorithm is derived from this gist: https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba
which is also referenced by Haskell's `Data.ZigZag` library used in uplc.

```k
  syntax Int ::= #decodeZigZag( Int ) [function]
//----------------------------------------------
  rule #decodeZigZag( I ) => ( I >>Int 1 )
  requires I %Int 2 ==Int 0

  rule #decodeZigZag( I ) => (~Int I) >>Int 1 [owise]

```

### Reading ByteString Values

```k
  syntax String ::= #readStringValue( BitStream ) [function]
//----------------------------------------------------------
  rule #readStringValue( BitStream( I, Bs ) ) =>
   #let
     StartIndex = (I /Int 8 ) +Int 1
   #in
     #readStringValue( Bs, StartIndex, StartIndex +Int #readNBits( 8, BitStream( I , Bs ) ) )

  syntax String ::= #readStringValue( BytesData:Bytes, StartByte:Int, ByteLength:Int ) [function]
//-------------------------------------------------------------------------------------------------
  rule #readStringValue( Bytes, Start, Length ) => #decodeUtf8Bytes( substrBytes( Bytes, Start, Length ) )

  syntax ByteString ::= #readByteStringValue( BitStream ) [function]
//------------------------------------------------------------------
  rule #readByteStringValue( BitStream( I, Bs ) ) =>
   #let
     StartIndex = (I /Int 8 ) +Int 1
   #in
     String2ByteString( "#" +String #readBytesAsString( Bs, StartIndex, StartIndex +Int #readNBits( 8, BitStream( I , Bs ) ) ) )

  syntax String ::= #readBytesAsString( BytesData:Bytes, StartByte:Int, ByteLength:Int ) [function]
//-------------------------------------------------------------------------------------------------
  rule #readBytesAsString( Bytes, Start, Length ) => Bytes2StringBase16( substrBytes( Bytes, Start, Length ) )

endmodule
```

The modules below split out Utf8 decoding functions between llvm backend and haskell backend as a temporary hack. Once
the `decodeBytes` has been implemented in the llvm backend, remove these modules and use decodeBytes for both backends.

```k
module DECODE-UTF8-BYTES-SYMBOLIC [symbolic]
  imports BYTES
  imports STRING

  syntax String ::= #decodeUtf8Bytes( Bytes ) [function]
  rule #decodeUtf8Bytes( Bs ) => decodeBytes( "UTF-8", Bs )

endmodule

module DECODE-UTF8-BYTES [concrete]
  imports BYTES
  imports FLAT-STRING-HELPER
  imports STRING
  imports UPLC-STRING

  syntax String ::= #decodeUtf8Bytes( Bytes ) [function]
  rule #decodeUtf8Bytes( Bs ) => #decodeUtf8( String2ByteString( "#" +String Bytes2StringBase16( Bs ) ) )

endmodule
```

String helper functions to translate between Bytes and String without losing leading zeros.

```k
module FLAT-STRING-HELPER
  imports BOOL
  imports BYTES
  imports INT
  imports STRING
  imports STRING-BUFFER

  syntax String ::= Bytes2StringBase16( Bytes ) [function]
  syntax String ::= Bytes2StringBase16( Bytes, Int, StringBuffer ) [function]

  rule Bytes2StringBase16( Bs ) => Bytes2StringBase16( Bs, 0, .StringBuffer )

  rule Bytes2StringBase16( Bs, I, Buffer ) => StringBuffer2String( Buffer )
    requires I ==Int lengthBytes( Bs )

  rule Bytes2StringBase16( Bs, I, Buffer ) => Bytes2StringBase16( Bs, I +Int 1, Buffer +String ("0" +String Base2String(Bs[I], 16) ) )
    requires I <Int lengthBytes( Bs ) andBool Bs[I] <Int 16

  rule Bytes2StringBase16( Bs, I, Buffer ) => Bytes2StringBase16( Bs, I +Int 1, Buffer +String Base2String(Bs[I], 16) ) [owise]

endmodule
```
