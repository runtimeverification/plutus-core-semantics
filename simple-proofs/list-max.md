# List-max: correctness claims

```k
requires "verification.md"

module LIST-MAX
  imports VERIFICATION
```

## Main claims

The correctness claim for the tail-recursive implementation of the list-max
function is as follows:

```k
  claim
    <k> [ LIST_MAX (con list(integer) [ X:Int, XS:ConstantList ]) ]
     => < con integer #maxList( X, XS ) >
     ...
    </k>
    <env> _ => .Map </env>
    requires allInts(XS)
```

This claim states that the execution of the list-max function with
an arbitrary list of integers `X, XS` passed in as parameter and starting
from an arbitrary environment terminates, returning the max of all of
the elements of `X, XS` (via the auxiliary function `#maxList(X, XS)`)
and an empty environment. Observe that we have to place an additional condition
which states that all of the elements of `XS` are indeed integers
(via the auxiliary function
[`allInts(XS)`](verification.md#allintsxs-capturing-that-xs-is-a-list-of-integers))).

## Invariants

In order to prove this claim, we need to prove the corresponding
invariant:

```k
  claim
    <k>
      LIST_MAX_TAIL_LOOP
        =>
      < con integer #maxList(AC, XS) > ...
    </k>
    <env>
      .Map
        [ f_lstMaxTail <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO [ s_0 <- REC_BODY_V RHO ] > ]
        [ ac_0 <- < con integer AC:Int > ]
        [ rest_0 <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstMaxTail LIST_MAX_TAIL_BODY .Map > ]
     andBool allInts(XS)
```

which follows the same structure as the invariant shown for recursive
[list-length](list-length.md#tail-recursive-list-length).


```k
endmodule
```
