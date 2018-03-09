module HEX-CONVERSION
  imports INT
  imports STRING

  syntax String ::= #toHex(Int)                                      [function]
                  | #hex(Int)                                        [function]

  rule #hex(0)  => "0"
  rule #hex(1)  => "1"
  rule #hex(2)  => "2"
  rule #hex(3)  => "3"
  rule #hex(4)  => "4"
  rule #hex(5)  => "5"
  rule #hex(6)  => "6"
  rule #hex(7)  => "7"
  rule #hex(8)  => "8"
  rule #hex(9)  => "9"
  rule #hex(10) => "A"
  rule #hex(11) => "B"
  rule #hex(12) => "C"
  rule #hex(13) => "D"
  rule #hex(14) => "E"
  rule #hex(15) => "F"

  // This will fail on 0, so should not be used by itself. See the rules for
  // `(builtin intToByteString (...))`.
  rule #toHex(0) => ""
  rule #toHex(N:Int) => #toHex(N /Int 16) +String #hex(N %Int 16)
                          when N =/=Int 0

endmodule
