```k
requires "verification.k"

module CBOR-UNIT-TEST
  imports VERIFICATION

  claim <k> simplify( DHead( 0, BitStream( 8, String2Bytes( "\x00" ) ) ) ) =>
    simplified( DH( BitStream( 8 , b"\x00" ) , 0 , 0 ) ) </k>

  claim <k> simplify( DHead( 25 , BitStream( 8, String2Bytes( "\x19\x01\x00" ) ) ) ) =>
    simplified( DH( BitStream( 24 , b"\x19\x01\x00" ) , 0 , 256 ) ) </k>

  claim <k> simplify( DData( String2Bytes( "\x00" ) ) ) =>
    simplified( BTPair( BitStream ( 8 , b"\x00" ) , Integer 0 ) ) </k>

  claim <k> simplify( DData( String2Bytes( "\x19\x01\x00" ) ) ) =>
    simplified( BTPair( BitStream ( 24 , b"\x19\x01\x00" ) , Integer 256 ) ) </k>

  claim <k> simplify( DData( String2Bytes( "\x3a\x3a\xde\x68\xb0" ) ) ) =>
    simplified( BTPair( BitStream ( 40 , b"::\xdeh\xb0" ) , Integer -987654321 ) ) </k>
```

ByteStrings of length 0 - 64

```k
  claim <k> simplify( DBlock( BitStream( 0, String2Bytes( "\x40" ) ) ) ) =>
    simplified( BBPair( BitStream ( 8 , b"\x40" ) , String2Bytes("") ) ) </k>

  claim <k> simplify( DBlock( BitStream( 0, String2Bytes( "\x58\x40" ) +Bytes Int2Bytes( 64, 1234567890, BE ) ) ) ) =>
    simplified( BBPair(
                  BitStream( 528 , String2Bytes( "\x58\x40" ) +Bytes Int2Bytes( 64, 1234567890, BE )),
                  Int2Bytes( 64, 1234567890, BE ) ) ) </k>
```

ByteStrings longer than 64 bytes

```k
  claim <k> simplify( DBlocks( BitStream( 0, #65_BYTES_OF_0 ) ) ) =>
            simplified( BBPair( BitStream ( 552 , #65_BYTES_OF_0 ), Int2Bytes( 65, 0, BE ) ) ) </k>

  claim <k> simplify( DBStar( BitStream( 0, #65_BYTES_OF_0_WITH_HEAD ) ) ) =>
            simplified( BBPair( BitStream ( 560, #65_BYTES_OF_0_WITH_HEAD ), Int2Bytes( 65, 0, BE ) ) ) </k>
```

Integers with size larger than 64 bits.

```k
  claim <k> simplify( DData( #LARGE_POSITIVE_INT ) ) =>
            simplified( BTPair( BitStream( lengthBytes( #LARGE_POSITIVE_INT ) *Int 8 , #LARGE_POSITIVE_INT ),
                                Integer 18446744073709551616 ) ) </k>
  claim <k> simplify( DData( #LARGE_NEGATIVE_INT ) ) =>
            simplified( BTPair( BitStream( lengthBytes( #LARGE_NEGATIVE_INT ) *Int 8 , #LARGE_NEGATIVE_INT ),
                                Integer -18446744073709551617 ) ) </k>
endmodule
```
