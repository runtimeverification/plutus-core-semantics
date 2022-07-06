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
```
 Removing the use of syntactic lists somehow makes this claim pass

```k
  claim <k> [ ( lam v_0 v_0 ) (T:Term) ] => T ~> [ < lam v_0 v_0 M > _] ... </k>
        <env> M </env>
```

The following two claims proves that the constant 1 applied to the identity function returns the
constant integer 1.

```k
  claim <k> [ ( lam v_0 v_0 ) (con integer 1) ] => < con integer 1 > ... </k>
        <env> RHO => ?_RHO </env>
  requires allValuesAreList(RHO) andBool (v_0 in_keys(RHO))

  claim <k> [ ( lam v_0 v_0 ) (con integer 1) ] => < con integer 1 > ... </k>
        <env> RHO => ?_RHO </env>
  requires allValuesAreList(RHO) andBool notBool(v_0 in_keys(RHO))

endmodule
```
