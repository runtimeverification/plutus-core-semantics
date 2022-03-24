requires "uplc.md"

module VERIFICATION
  imports UPLC

  syntax KItem ::= simplify(KItem)
                 | simplified(KItem)

  rule <k> simplify(K) => simplified(K) ... </k>

endmodule
