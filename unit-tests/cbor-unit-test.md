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

endmodule
```
