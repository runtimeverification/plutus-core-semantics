```k
requires "verification.k"

module SIMPLE-SYMBOLIC
  imports VERIFICATION

//  claim <k> ( program 1.0.0 [ [ (builtin addInteger) (con integer X)] (con integer Y)] ) => [] ( con integer Y +Int X ) </k>

  claim <k> [ [ (builtin addInteger) (con integer X)] (con integer Y)] =>
            < con integer Y +Int X > ... </k>


endmodule
```
