```k
requires "verification.k"

module SIMPLE-SYMBOLIC
  imports VERIFICATION
  imports K-EQUAL

  claim <k> [ [ (builtin addInteger) (con integer X)] (con integer Y)] =>
            < con integer Y +Int X > ... </k>

  claim <k> [ (builtin _:IntegerBuiltinName) (con TN:TypeConstant _) ] =>
            (error) </k>
  requires TN =/=K integer

  claim <k> [ [ (builtin _:IntegerBuiltinName) (con integer _) ] (con TN _ )] =>
            (error) </k>
  requires TN =/=K integer

  claim <k> [ [ (builtin _:PolyBuiltinName) _:Term ]
                                            _:Term ] ~> T:Term =>
            (error) </k>
  requires T =/=K Force

  // Removing the use of syntactic lists somehow makes this claim pass
  claim <k> [ ( lam v_0 v_0 ) (T:Term) ] => T ~> [ < lam v_0 v_0 M > _] ... </k>
        <env> M </env>

  claim <k> [ ( lam v_0 v_0 ) (con integer 1) ] => (con integer 1) ... </k>

  claim <k> [ ( lam v_0 v_0 ) (con TN:TypeConstant C:Constant) ] => ( con TN C ) ... </k>

  claim <k> [ ( lam X:UplcId X ) (T:Term)] => ( T ) ... </k>

endmodule
```
