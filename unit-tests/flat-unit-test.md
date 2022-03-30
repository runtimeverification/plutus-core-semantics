```k
requires "verification.k"

module FLAT-UNIT-TEST
  imports VERIFICATION

  claim <k> simplify(#bit2boolval(0)) => simplified(False) </k>

  claim <k> simplify( #getVarLenData( BitStream ( 0, String2Bytes( "\x01" ) ) ) )
     => simplified( 1 ) </k>

  claim <k> simplify( #getVarLenData( BitStream ( 1, String2Bytes( "\x01\x01" ) ) ) )
     => simplified( 2 ) </k>

  claim <k> simplify( #getVarLenData( BitStream ( 8, String2Bytes( "\x01\x02" ) ) ) )
     => simplified( 2 ) </k>

  claim <k> simplify( #getVarLenData( BitStream ( 0, String2Bytes( "\x86\x04" ) ) ) )
     => simplified( 6 +Int (4 <<Int 7) ) </k>

  claim <k> simplify( #getVarLenData( BitStream( 0, String2Bytes( "\x80\x80\x02" ) ) ) )
    => simplified( 2 <<Int ( 7 *Int 2 ) ) </k>

endmodule
```
