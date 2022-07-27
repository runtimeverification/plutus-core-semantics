requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  syntax KItem ::= simplify(KItem)
                 | simplified(KItem)

  rule <k> simplify(K) => simplified(K) ... </k>

  syntax Bytes ::= "#65_BYTES_OF_0" [macro]
  rule #65_BYTES_OF_0 => String2Bytes( "\x58\x40" ) +Bytes Int2Bytes( 64, 0, BE ) +Bytes String2Bytes( "\x41\x00\xff" )

  syntax Bytes ::= "#65_BYTES_OF_0_WITH_HEAD" [macro]
  rule #65_BYTES_OF_0_WITH_HEAD => String2Bytes( "\x5f" ) +Bytes #65_BYTES_OF_0

  syntax Bytes ::= "#LARGE_POSITIVE_INT" [macro]
  rule #LARGE_POSITIVE_INT => String2Bytes( "\xc2\x49\x01\x00\x00\x00\x00\x00\x00\x00\x00" )

  syntax Bytes ::= "#LARGE_NEGATIVE_INT" [macro]
  rule #LARGE_NEGATIVE_INT => String2Bytes( "\xc3\x49\x01\x00\x00\x00\x00\x00\x00\x00\x00" )
endmodule
