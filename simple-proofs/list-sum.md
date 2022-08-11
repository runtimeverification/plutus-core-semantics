# List-sum: correctness claims

```k
requires "verification.md"

module LIST-SUM
  imports VERIFICATION
```

## Main claims

The correctness claims for the two recursive implementations of the list-sum
function are as follows:

```k
  claim
    <k>
      [ LIST_SUM_Z (con list(integer) [ XS:ConstantList ]) ]
        =>
      < con integer sum(XS) > ...
    </k>
    <env> _ => .Map </env>
    requires allInts(XS)

  claim
    <k>
      [ LIST_SUM_REC (con list(integer) [ XS:ConstantList ]) ]
        =>
      < con integer sum(XS) > ...
    </k>
    <env> _ => .Map </env>
    requires allInts(XS)
```

These claims state that the execution of the list-sum function with
an arbitrary list of integers `XS` passed in as parameter and starting
from an arbitrary environment terminates, returning the sum of all of
the elements of `XS` (via the auxiliary function
[`sum(XS)`](verification.md#sumxs-calculating-the-sum-of-a-given-list-of-integers-xs))
and an empty environment. Observe that, unlike in the previous examples,
here we have to place an additional condition
which states that all of the elements of `XS` are indeed integers
(via the auxiliary function
[`allInts(XS)`](verification.md#allintsxs-capturing-that-xs-is-a-list-of-integers))).

## Invariants

In order to prove these claims, we need to prove the corresponding
invariants:

```k
  claim
    <k>
      LIST_SUM_LOOP
        =>
      < con integer sum(XS) > ...
    </k>
    <env>
      .Map:Map
        [ f_lstSum <- < lam x_0 [ s_0 s_0 x_0 ] RHO [ s_0 <- Z_BODY_V RHO ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstSum LIST_SUM_BODY .Map > ]
     andBool allInts(XS)

  claim
    <k>
      LIST_SUM_LOOP
        =>
      < con integer sum(XS) > ...
    </k>
    <env>
      .Map:Map
        [ f_lstSum <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO [ s_0 <- REC_BODY_V RHO ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstSum LIST_SUM_BODY .Map > ]
     andBool allInts(XS)
```

which follow the same structure as the previously shown invariants for
[list-free](list-free.md#invariants), [list-length](list-length.md#invariants),
and [list-longer](list.longer.md#invariants).

```k
endmodule
```