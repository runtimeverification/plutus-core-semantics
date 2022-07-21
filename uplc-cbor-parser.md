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
  syntax BitStreamTextualPair ::= BTPair( BitStream, TextualData )

  syntax BitStreamTextualPair ::= DData( Bytes ) [function]
  syntax BitStreamTextualPair ::= DData( DHeadReturnValue ) [function]
//-----------------------------------------------------------------------------
  rule DData( CborBytes ) => DData( DHead( CborBytes[0], BitStream( 8, CborBytes ) ) )

  rule DData( DH( S, 0, N ) ) => BTPair( S, Integer N )

```

```k
endmodule
```
