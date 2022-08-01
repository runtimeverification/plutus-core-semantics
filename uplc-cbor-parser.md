# CBOR Decoder for UPLC's Data Constant

```k
requires "bitstream.md"
requires "uplc-flat-parser.md"
requires "uplc-syntax.md"

module UPLC-CBOR-PARSER
  imports BITSTREAM
  imports BYTES
  imports FLAT-STRING-HELPER
  imports INT
  imports UPLC-SYNTAX
```

d( K, S )
---------

A function which decodes a k-byte natural number from the start of a bytestring.

```k
  syntax BitStreamIntPair ::= BIPair( BitStream, Int )

  syntax BitStreamIntPair ::= d(k:Int, s:BitStream) [function]
//------------------------------------------------------------
  rule d( K, S ) =>
    #let
      BitLen = K *Int 8
    #in
      BIPair( #advancePosNBits( BitLen, S ) , #readNBits( BitLen, S ) )
```

DHead( N, S )
-------------

A function which decodes the head of a cbor encoding. Takes the first byte N and a bytestring and decodes
to the remaining bytestring, the major type and the argument of the head.

```k
  syntax DHeadReturnValue ::= DH( CborData:BitStream, MajorType:Int, Arg:Int)

  syntax DHeadReturnValue ::= DHead( Int, BitStream ) [function]
//--------------------------------------------------------------
  rule DHead( N , S ) =>
    #let
      BIPair( S1, K ) = d( 2 ^Int ( N %Int 32 -Int 24 ) , S )
    #in
      DH( S1, N /Int 32, K )
  requires 24 <=Int N %Int 32
   andBool N %Int 32 <=Int 27

  rule DHead( N , S ) => DH( S, N /Int 32, N %Int 32 )
  requires N %Int 32 <=Int 23
```

DIndef( N, S )
--------------

Decode heads for indefinite items.

```k
  syntax BitStreamIntPair ::= DIndef( Int, BitStream ) [function]
//---------------------------------------------------------------
  rule DIndef( N, S ) =>
    #let
      M = ( N -Int 31 ) /Int 32
    #in
      BIPair( S, M )
  requires 2 <=Int ( N -Int 31 ) /Int 32
   andBool ( N -Int 31 ) /Int 32 <=Int 5
```

DnBytes( N, S )
---------------

Read N bytes from S.

```k
  syntax BitStreamBytesPair ::= BBPair( BitStream, Bytes )

  syntax BitStreamBytesPair ::= DnBytes( Int, BitStream ) [function]
//------------------------------------------------------------------
  rule DnBytes( N, S ) =>
    #let
      BitLen = N *Int 8
    #in
      #let
        Data = #readNBits( BitLen, S )
      #in
        BBPair( #advancePosNBits(BitLen, S), Int2Bytes(N, Data, BE) )
```

DBlock( S )
-----------

Extract a bytestring of length at most 64.

```k
  syntax BitStreamBytesPair ::= DBlock( BitStream ) [function]
//------------------------------------------------------------
  rule DBlock( S ) => DBlock( DHead( #readNBits( 8, S ), #advancePosNBits( 8, S ) ) )

  syntax BitStreamBytesPair ::= DBlock( DHeadReturnValue ) [function]
//-------------------------------------------------------------------
  rule DBlock( DH( S1, BYTESTRING_TYPE, N ) ) => DnBytes( N, S1 )
    requires N <=Int 64
```

DBlocks( S )
------------

Decode a sequence of blocks.

```k
  syntax BitStreamBytesPair ::= DBlocks( BitStream ) [function]
//-------------------------------------------------------------
  rule DBlocks( S ) => DBlocks( S, .Bytes )

  syntax BitStreamBytesPair ::= DBlocks( BitStream, Bytes ) [function]
//--------------------------------------------------------------------
  rule DBlocks( S, Bs ) =>
    #let
      BBPair( Bits, DataBytes ) = DBlock( S )
    #in
      DBlocks( Bits, Bs +Bytes DataBytes )

  rule DBlocks( S, Bs ) => BBPair( #advancePosNBits( 8, S ) , Bs +Bytes .Bytes )
    requires #readNBits( 8, S ) ==Int INDEF_TERMINATOR

  syntax Int ::= "INDEF_TERMINATOR" [macro]
  rule INDEF_TERMINATOR => 255
```


DBStar( S )
-----------

Top-level parsing function for bytestrings.

```k
  syntax BitStreamBytesPair ::= DBStar( BitStream ) [function]
//------------------------------------------------------------
  rule DBStar( Bs ) => DBStar( #readNBits( 8, Bs ), Bs )

  syntax BitStreamBytesPair ::= DBStar( Int,  BitStream ) [function]
//------------------------------------------------------------------
  rule DBStar( Head, Bs ) => DBlocks( #advancePosNBits( 8, Bs ) )
    requires ( Head -Int 31 ) /Int 32 ==Int 2

  rule DBStar( _, Bs ) => DBlock( Bs )
    [owise]
```

DZ( S )
-------

Decode an integer.

```k
  syntax BitStreamIntPair ::= DZ( BitStream ) [function]
//------------------------------------------------------
  rule DZ( S ) => DZ( DHead( #readNBits( 8, S ), #advancePosNBits( 8, S ) ) )

  syntax BitStreamIntPair ::= DZ( DHeadReturnValue ) [function]
//------------------------------------------------------

  rule DZ( DH( S, UNSIGNED_INT_TYPE, N ) ) => BIPair( S, N )
  rule DZ( DH( S, NEGATIVE_INT_TYPE, N ) ) => BIPair( S, -1 *Int N -Int 1 )
```

Integers that span over 64 bits.

```k
  rule DZ( DH( S, TAG_TYPE, POSITIVE_INT_TAG_NUMBER) ) =>
    #let
      BBPair( S1, B ) = DBStar( S )
    #in
      BIPair( S1, Bytes2Int( B, BE, Unsigned ) )

  rule DZ( DH( S, TAG_TYPE, NEGATIVE_INT_TAG_NUMBER ) ) =>
    #let
      BBPair( S1, B ) = DBStar( S )
    #in
      BIPair( S1, -1 *Int Bytes2Int( B, BE, Unsigned ) -Int 1 )
```

DData( S )
----------

Top-level function that decodes a CBOR bytestring to a pair of BitStream and TextualData. The sepc defines
this function in a way that's not friendly for K and a straight translation will likely not work. This
implementation matches on the major type and their argument returned by DHead to determine the constructor
to decode.

```k
  syntax Int ::= "UNSIGNED_INT_TYPE" [macro]
               | "NEGATIVE_INT_TYPE" [macro]
               | "BYTESTRING_TYPE"   [macro]
               | "ARRAY_TYPE"        [macro]
               | "TAG_TYPE"          [macro]

  rule UNSIGNED_INT_TYPE => 0
  rule NEGATIVE_INT_TYPE => 1
  rule BYTESTRING_TYPE   => 2
  rule ARRAY_TYPE        => 4
  rule TAG_TYPE          => 6
```

TAG_TYPE has a "tag number"

```k
  syntax Int ::= "POSITIVE_INT_TAG_NUMBER" [macro]
               | "NEGATIVE_INT_TAG_NUMBER" [macro]

  rule POSITIVE_INT_TAG_NUMBER => 2
  rule NEGATIVE_INT_TAG_NUMBER => 3
```

```k
  syntax BitStreamTextualPair ::= BTPair( BitStream, TextualData )

  syntax BitStreamTextualPair ::= DData( BitStream ) [function]
  syntax BitStreamTextualPair ::= DData( DHeadReturnValue, BitStream ) [function]
//---------------------------------------------------------------------------
  rule DData( S ) => DData( DHead( #readNBits( 8 , S ), #advancePosNBits( 8, S ) ), S )
```

Parsing an Integer data:

```k
  rule DData( DH(  _, MAJOR_TYPE, ARGUMENT ), S ) =>
    #let
      BIPair( S1, N ) = DZ( S )
    #in
      BTPair( S1, Integer N )
  requires MAJOR_TYPE ==Int UNSIGNED_INT_TYPE orBool
           MAJOR_TYPE ==Int NEGATIVE_INT_TYPE orBool
           (MAJOR_TYPE ==Int TAG_TYPE andBool ARGUMENT ==Int POSITIVE_INT_TAG_NUMBER) orBool
           (MAJOR_TYPE ==Int TAG_TYPE andBool ARGUMENT ==Int NEGATIVE_INT_TAG_NUMBER)
```

Parsing a ByteString data:

```k
  rule DData( DH(  _, MAJOR_TYPE, _ ), S ) =>
    #let
      BBPair( S1, B ) = DBStar( S )
    #in
      BTPair( S1, ByteString String2ByteString( "#" +String Bytes2StringBase16( B ) ) )
  requires MAJOR_TYPE ==Int BYTESTRING_TYPE orBool
           ( #readNBits( 8, S ) -Int 31 ) /Int 32 ==Int 2
```

DDataStar( S )
--------------

Decode a `List` constructor.

```k
  syntax BitStreamTextualPair ::= DDataStar( BitStream ) [function]
//-----------------------------------------------------------------
  rule DDataStar( S ) =>
  #let
    HEAD_INT = #readNBits( 8, S )
  #in
    #let
      REST = #advancePosNBits( 8, S )
    #in
      DDataStar( DHead( HEAD_INT, REST ), DIndef( HEAD_INT, REST ) )

  syntax BitStreamTextualPair ::= DDataStar( DHeadReturnValue, BitStreamIntPair ) [function]
//------------------------------------------------------------------------------------------
  rule DDataStar( DH( S1, ARRAY_TYPE, N ), _ ) => DDataNStar( N, S1 )
  rule DDataStar( _, BIPair( S1, 4 ) ) => DDataIndefStar( S1 )
```

DDataNStar( N, S )
------------------

Decode a list of N items

```k
  syntax BitStreamTextualPair ::= DDataNStar( Int, BitStream ) [function]
//-----------------------------------------------------------------------
  rule DDataNStar( 0, S ) => BTPair( S, List [ .DataList ] )

  rule DDataNStar( N, S ) =>
    #let
      BTPair( S1, D ) = DData( S )
    #in
      #let
        BTPair( S2, List [ L ]  ) = DDataNStar( N -Int 1, S1 )
      #in
        BTPair( S2, List [ D , L ] )
```

DDataIndefStar( S )
-------------------

Decode a list of indefinite items.

```k
  syntax BitStreamTextualPair ::= DDataIndefStar( BitStream ) [function]
//----------------------------------------------------------------------
  rule DDataIndefStar( S ) => BTPair( S, List [ .DataList ] )
    requires #readNBits( 8, S ) ==Int INDEF_TERMINATOR

  rule DDataIndefStar( S ) =>
    #let
      BTPair( S1, D ) = DData( S )
    #in
      #let
        BTPair( S2, List [ L ]  ) = DDataIndefStar( S1 )
      #in
        BTPair( S2, List [ D , L ] )
```

DData2NStar( N, S )
-------------------

Decode a Map where each element is a pair of data.

```k
  syntax BitStreamTextualPair ::= DData2NStar( Int, BitStream ) [function]
//------------------------------------------------------------------------
  rule DData2NStar( 0, S ) => BTPair( S, Map [ .DataPairList ] )

  rule DData2NStar( N, S ) =>
    #let
      BTPair( S1, K ) = DData( S )
    #in
      #let
        BTPair( S2, D ) = DData( S1 )
      #in
        #let
          BTPair( S3, Map [ DataPair ] ) = DData2NStar( N -Int 1, S2)
        #in
          BTPair( S3, Map [ ( K, D ) , DataPair ] )
```

DCTag( S )
----------

Decode a `Contr Int [ Data ]` constructor.

```k
  syntax BitStreamIntPair ::= DCTag( BitStream ) [function]
//---------------------------------------------------------
  rule DCTag( S ) => DCTag( DHead( #readNBits( 8, S ), #advancePosNBits( 8, S ) ) )

  syntax BitStreamIntPair ::= DCTag( DHeadReturnValue ) [function]
//----------------------------------------------------------------
  rule DCTag( DH( S1, TAG_TYPE, I ) ) => BIPair( S1, I -Int 121 )
    requires 121 <=Int I
     andBool I <=Int 127

  rule DCTag( DH( S1, TAG_TYPE, I ) ) => BIPair( S1, I -Int 1280 +Int 7 )
    requires 1280 <=Int I
     andBool I <=Int 1400

  rule DCTag( DH( S1, TAG_TYPE, 102 ) ) => DCTag( DHead( #readNBits( 8, S1 ), #advancePosNBits( 8, S1 ) ) )

  rule DCTag( DH( S2, ARRAY_TYPE, 2 ) ) => DCTag( DZ( S2 ) )

  syntax BitStreamIntPair ::= DCTag( BitStreamIntPair ) [function]
//----------------------------------------------------------------
  rule DCTag( BIPair( S3, I ) ) => BIPair( S3, I )
    requires 0 <=Int I
     andBool I <=Int (2 ^Int 64) -Int 1
```

DecodeCborData( Bs )
--------------------

This function converts the input ByteString to a Bytes sort and calls `DData` with Bytes as the parameter.

```k
  syntax TextualData ::= DecodeCborData( ByteString ) [function]
  rule DecodeCborData( Bs ) =>
    #let
      INPUT = trimByteString( Bs )
    #in
      #let
        INPUT_BYTES = Int2Bytes( lengthString( INPUT ) /Int 2, String2Base( INPUT, 16 ), BE )
      #in
        #let
          BTPair( _, T ) = DData( BitStream( 0, INPUT_BYTES ) )
        #in
          T
```

#DecodeCborByteStrings( Program )
---------------------------------

Decode function called after program load. This function traverses the entire input program and decodes
any CBOR data constants to the textualData format. By the time the rewrite rules are applied, all data
constants would be expressed as TextualData.

```k
  syntax Program ::= #decodeCBORBytestrings( Program ) [function]
  syntax Term ::= #decodeCBORBytestrings( Term ) [function]

  rule #decodeCBORBytestrings( ( program V:Version T:Term ) ) => ( program V #decodeCBORBytestrings( T ) )

  rule #decodeCBORBytestrings( ( con data Bs:ByteString ) ) => ( con data { DecodeCborData(Bs) } )
  rule #decodeCBORBytestrings( ( con T:TypeConstant C:Constant ) ) => ( con T C ) [owise]
  rule #decodeCBORBytestrings( U:UplcId ) => U
  rule #decodeCBORBytestrings( ( error ) ) => ( error )
  rule #decodeCBORBytestrings( ( delay T ) ) => ( delay #decodeCBORBytestrings( T ) )
  rule #decodeCBORBytestrings( ( force T ) ) => ( force #decodeCBORBytestrings( T ) )
  rule #decodeCBORBytestrings( ( builtin Bn:BuiltinName ) ) => ( builtin Bn )
  rule #decodeCBORBytestrings( ( lam U:UplcId T:Term ) ) => ( lam U #decodeCBORBytestrings( T ) )
  rule #decodeCBORBytestrings( [ T Ts ] ) => [ #decodeCBORBytestrings( T ) #decodeCBORBytestrings( Ts ) ]

  syntax TermList ::= #decodeCBORBytestrings( TermList ) [function]
  rule #decodeCBORBytestrings( T:Term Ts:TermList ) => #decodeCBORBytestrings( T ) #decodeCBORBytestrings( Ts )
```

```k
endmodule
```
