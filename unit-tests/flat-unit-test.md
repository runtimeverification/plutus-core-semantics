```k
requires "verification.k"

module FLAT-UNIT-TEST
  imports VERIFICATION

  claim <k> simplify(#bit2boolval(0)) => simplified(False) </k>

endmodule
```
