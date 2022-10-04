```k
requires "verification.md"

module SIMPLE-SYMBOLIC
  imports VERIFICATION
  imports K-EQUAL

  claim <k> [ [ (builtin addInteger) (con integer X)] (con integer Y)] =>
            < con integer Y +Int X > ... </k>
        <env> _ => .Map </env>

  claim <k> [ (builtin _:IntegerBuiltinName) (con TN:TypeConstant _) ] =>
            (error) </k>
        <env> _ => .Map </env>
  requires TN =/=K integer

  claim <k> [ [ (builtin _:IntegerBuiltinName) (con integer _) ] (con TN _ )] =>
            (error) </k>
        <env> _ => .Map </env>
  requires TN =/=K integer
```
 Removing the use of syntactic lists somehow makes this claim pass

```k
  claim <k> [ ( lam v_0 v_0 ) (T:Term) ] => T ~> [ < lam v_0 v_0 .Map > _] ... </k>
        <env> M => #cutEnv(M, T) </env>
        requires #closed(T)
```

The following claim proves that the constant 1 applied to the identity function returns the
constant integer 1.

```k
  claim <k> [ ( lam v_0 v_0 ) (con integer 1) ] => < con integer 1 > ... </k>
        <env> _ => .Map </env>

endmodule
```