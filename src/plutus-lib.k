module PLUTUS-LIB

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

  syntax Int ::= #log2Int(Int)                       [function, smtlib(logInt)]

  rule #log2Int(I) => #log2Int(0 -Int I) when I <Int 0
  rule #log2Int(0) => 1
  rule #log2Int(1) => 1
  rule #log2Int(I) => 1 +Int #log2Int(I /Int 2) when I >Int 1

  rule #log2Int(_) >=Int 0 => true [smt-lemma]

  // rule #log2Int(0) ==Int 0 => true [smt-lemma]
  // rule #log2Int(1) ==Int 0 => true [smt-lemma]

  // rule #log2Int(I) ==Int 1 +Int #log2Int(I /Int 2) => I >Int 1 [smt-lemma]

endmodule
