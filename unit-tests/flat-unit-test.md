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

  claim <k> simplify( #readIntegerValue( BitStream ( 0, String2Bytes( "\x16" ) ) ) )
    => simplified( VDat( 8, 11 ) ) ... </k>

  claim <k> simplify( #readProgramTerm( #readTerm, BitStream( 0, String2Bytes( "\x48\x05\x81" ) ) ) )
    => simplified( ( con integer 11 ) ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x0b\x16\x21\x48\x05\x81" ) ) )
    => simplified( ( program 11.22.33 ( con integer 11) ) ) ... </k>

  claim <k> simplify( #readBytesAsString( String2Bytes( "\xff" ), 0, 1 ) )
    => simplified( "ff" ) ... </k>

  claim <k> simplify( #readBytesAsString( String2Bytes( "\x00\xff" ), 0, 2 ) )
    => simplified( "00ff" ) ... </k>

  claim <k> simplify( #readBytesAsString( String2Bytes( "\x01\xff" ), 0, 2 ) )
    => simplified( "01ff" ) ... </k>

  claim <k> simplify( #readBytesAsString( String2Bytes( "\x01\x0f" ), 0, 2 ) )
    => simplified( "010f" ) ... </k>

  claim <k> simplify( #readByteStringValue( BitStream( 0, String2Bytes( "\x02\x00\xff" ) ) ) )
    => simplified( String2ByteString("#00ff") ) ... </k>

  claim <k> simplify( #readByteStringValue( BitStream( 0, String2Bytes( "\x00\xff" ) ) ) )
    => simplified( String2ByteString("#") ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x48\x81\x01\xff\x00\x01" ) ) )
    => simplified( ( program 1.0.0 ( con bytestring String2ByteString("#ff") ) ) ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x48\x81\x00\x01" ) ) )
    => simplified( ( program 1.0.0 ( con bytestring String2ByteString("#") ) ) ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x48\x81\x05\x00\x01\x0a\xf0\x04\x00\x01" ) ) )
    => simplified( ( program 1.0.0 ( con bytestring String2ByteString("#00010af004") ) ) ) ... </k>

  claim <k> simplify( #readStringValue( String2Bytes( "\x68\x65\x6c\x6c\x6f\x21" ), 0, 6 ) )
    => simplified( "hello!" ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x49\x01\x00\x01" ) ) )
    => simplified( ( program 1.0.0 ( con string "") ) ) ... </k>

  claim <k> simplify( #readProgramTerm( #readTerm, BitStream( 0, String2Bytes( "\x70\x00" ) ) ) )
    => simplified( #readProgramTerm( ( builtin addInteger ), BitStream( 11, String2Bytes( "\x70\x00" ) ) ) ) ... </k>

  claim <k> simplify( #readProgramTerm( #readTerm, BitStream( 0, String2Bytes( "\x70\x20" ) ) ) )
    => simplified( #readProgramTerm( ( builtin subtractInteger), BitStream( 11, String2Bytes( "\x70\x20" ) ) ) ) ... </k>

  claim <k> simplify( #readProgramTerm( #readTerm, BitStream( 0, String2Bytes( "\x70\x20\x00" ) ) ) )
    => simplified( TermBitLengthPair( ( builtin subtractInteger), 11 ) ) ... </k>

  claim <k> simplify( #bytes2program( String2Bytes( "\x01\x00\x00\x33\x70\x29\x00\x12\x40\x09" ) ) )
    => simplified( ( program 1.0.0 [ [ ( builtin subtractInteger ) ( con integer 1 ) ] ( con integer 2 ) ] ) ) ... </k>


endmodule
```
