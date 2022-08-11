# List-longer: correctness claims

```k
requires "verification.md"

module LIST-LONGER
  imports VERIFICATION
```

## Main claims

The correctness claims for the two recursive implementations of the list-longer
function are as follows:

```k
  claim
    <k>
      [ LIST_LONGER_Z (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con bool longer(XS, YS) > ...
    </k>
    <env> _ => .Map </env>

  claim
    <k>
      [ LIST_LONGER_REC (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con bool longer(XS, YS) > ...
    </k>
    <env> _ => .Map </env>
```

These claims state that the execution of the list-longer function with
two arbitrary list of integers, `XS` and `YS`, passed in as parameters,
and starting from an arbitrary environment terminates, returns the uplc
Boolean `True` if `XS` is longer than `YS`, and `False` otherwise (via
the auxiliary function
[`longer(XS, YS)`](verification.md#longerlistxs-ys-capturing-that-the-list-xs-is-longer-than-the-list-ys)),
and an empty environment.

## Invariants

In order to prove these claims, we need to prove the corresponding
invariants:

```k
  claim
    <k>
      LIST_LONGER_LOOP
        =>
      < con bool longer(XS, YS) > ...
    </k>
    <env>
      .Map:Map
        [ f_lstLonger <- < lam x_0 [ s_0 s_0 x_0 ] .Map [ s_0 <- Z_BODY_V RHO ] > ]
        [ in_lst1 <- < con list ( integer ) [ XS:ConstantList ] > ]
        [ in_lst2 <- < con list ( integer ) [ YS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY .Map > ]

  claim
    <k>
      LIST_LONGER_LOOP
        =>
      < con bool longer(XS, YS) > ...
    </k>
    <env>
      .Map:Map
        [ f_lstLonger <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO [ s_0 <- REC_BODY_V RHO ] > ]
        [ in_lst1 <- < con list ( integer ) [ XS:ConstantList ] > ]
        [ in_lst2 <- < con list ( integer ) [ YS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY .Map > ]
```

which follow the same structure as the previously shown invariants for
[list-free](list-free.md#invariants) and [list-length](list-length.md#invariants),
with the addition of the second parameter (`in_lst2` in the initial store); further
parameters would follow analogously.

```k
endmodule
```