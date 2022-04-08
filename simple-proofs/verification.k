requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  syntax KItem ::= simplify(KItem)
                 | simplified(KItem)

  rule <k> simplify(K) => simplified(K) ... </k>

endmodule
