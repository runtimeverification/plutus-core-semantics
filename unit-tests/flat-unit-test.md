```k
requires "verification.k"

module FLAT-UNIT-TEST
  imports VERIFICATION

  claim <k> simplify(#bit2boolval(0)) => simplified(False) </k>

  claim <k> simplify( #getVarLenData( BitStream ( 0, String2Bytes( "\x01" ) ) ) )
     => simplified( VDat( 8, 1 ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream ( 1, String2Bytes( "\x01\x01" ) ) ) )
     => simplified( VDat( 8, 2 ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream ( 8, String2Bytes( "\x01\x02" ) ) ) )
     => simplified( VDat( 8, 2 ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream ( 0, String2Bytes( "\x86\x04" ) ) ) )
     => simplified( VDat( 16, 6 +Int (4 <<Int 7) ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream( 0, String2Bytes( "\x80\x80\x02" ) ) ) )
    => simplified( VDat( 24, 2 <<Int ( 7 *Int 2 ) ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream( 0, String2Bytes( "\x80\x01" ) ) ) )
    => simplified( VDat( 16, 128 ) ) ... </k>

  claim <k> simplify( #getVarLenData( BitStream( 0, String2Bytes( "\x01\x00" ) ) ) )
    => simplified( VDat( 8, 1 ) ) ... </k>

  claim <k> simplify( #readVersion( BitStream ( 0, String2Bytes( "\x01\x00\x00" ) ) ) )
    => simplified( SDat( 24, "1.0.0" ) ) ... </k>

  claim <k> simplify( #readVersion( BitStream ( 0, String2Bytes( "\x01\x02\x03" ) ) ) )
    => simplified( SDat( 24, "1.2.3" ) ) ... </k>

  claim <k> simplify( #readVersion( BitStream ( 0, String2Bytes( "\x80\x01\x01\x02" ) ) ) )
    => simplified( SDat( 32, "128.1.2" ) ) ... </k>

  claim <k> simplify( #readVersion( BitStream ( 0, String2Bytes( "\x80\x01\x80\x02\x80\x03" ) ) ) )
    => simplified( SDat( 48, "128.256.384" ) ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x49\x81" ) ) )
    => simplified( ( program 1.0.0 ( con unit ()) ) ) ... </k>

endmodule
```
