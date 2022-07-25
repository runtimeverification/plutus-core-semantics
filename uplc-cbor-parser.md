# CBOR Decoder for UPLC's Data Constant

```k
requires "bitstream.md"
requires "uplc-syntax.md"

module UPLC-CBOR-PARSER
  imports BITSTREAM
  imports BYTES
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

DData( S )
----------

Top-level function that decodes a CBOR bytestring to a pair of BitStream and TextualData. The sepc defines
this function in a way that's not friendly for K and a straight translation will likely not work. This
implementation matches on the major type returned by DHead to determine the constructor being decoded.

```k
  syntax Int ::= "UNSIGNED_INT_TYPE" [macro]
               | "NEGATIVE_INT_TYPE" [macro]

  rule UNSIGNED_INT_TYPE => 0
  rule NEGATIVE_INT_TYPE => 1
```

```k
  syntax BitStreamTextualPair ::= BTPair( BitStream, TextualData )

  syntax BitStreamTextualPair ::= DData( Bytes ) [function]
  syntax BitStreamTextualPair ::= DData( DHeadReturnValue ) [function]
//--------------------------------------------------------------------
  rule DData( CborBytes ) => DData( DHead( CborBytes[0], BitStream( 8, CborBytes ) ) )
  rule DData( DH( S, UNSIGNED_INT_TYPE, N ) ) => BTPair( S, Integer N )
  rule DData( DH( S, NEGATIVE_INT_TYPE, N ) ) => BTPair( S, Integer (-1 *Int N -Int 1) )
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
        BTPair( _, T ) = DData( Int2Bytes( lengthString( INPUT ) /Int 2, String2Base( INPUT, 16 ), BE) )
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
