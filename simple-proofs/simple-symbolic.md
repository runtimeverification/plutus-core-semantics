```k
requires "verification.k"

module SIMPLE-SYMBOLIC
  imports VERIFICATION
  imports K-EQUAL

  claim <k> [ [ (builtin addInteger) (con integer X)] (con integer Y)] =>
            < con integer Y +Int X > ... </k>

  claim <k> [ [ (builtin _:IntegerBuiltinName) (con TN1:TypeConstant _) ]
                                               (con TN2:TypeConstant _) ] =>
            (error) </k>
  requires TN1 =/=K integer orBool TN2 =/=K integer

  claim <k> [ [ (builtin _:PolyBuiltinName) _:Term ]
                                            _:Term ] ~> T:Term =>
            (error) </k>
  requires T =/=K Force 

endmodule
```
