# List-length: correctness claims

```k
requires "verification.md"

module LIST-LENGTH
  imports VERIFICATION
```

## Main claims

The correctness claims for the two recursive implementations of the list-length
function are as follows:

```k
  claim
    <k>
      [ LIST_LEN_Z (con list(integer) [ XS:ConstantList ]) ]
        =>
      < con integer length(XS) > ...
    </k>
    <env> _ => .Map </env>

  claim
    <k>
      [ LIST_LEN_REC (con list(integer) [ XS:ConstantList ]) ]
        =>
      < con integer length(XS) > ...
    </k>
    <env> _ => .Map </env>
```

These claims state that the execution of the list-length function with
an arbitrary list of integers `XS` passed in as parameter and starting
from an arbitrary environment terminates, returning the length of the
list `XS` (via the auxiliary function
[`length(XS)`](verification.md#lengthxs-calculating-the-length-of-a-given-list-xs))
and an empty environment.

## Invariants

In order to prove these claims, we need to prove the corresponding
invariants:

```k
  claim
    <k>
      LIST_LEN_LOOP
        =>
      < con integer length(XS) > ...
    </k>
    <env>
      RHO:Map
        [ f_lstLen <- < lam x_0 [ s_0 s_0 x_0 ] RHO_1 [ s_0 <- Z_BODY_V RHO_1 ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]

  claim
    <k>
      LIST_LEN_LOOP
        =>
      < con integer length(XS) > ...
    </k>
    <env>
      RHO:Map
        [ f_lstLen <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO_1 [ s_0 <- REC_BODY_V RHO_1 ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]
```

which, in comparison with the [list-free invariants](list-free.md#invariants),
exhibit only minor differences:
- `< con integer sum(XS) >` instead of `< con list(integer) [ .ConstantList ] >`
   (semantic difference, as the two functions behave differently);
- `f_lstSum` instead of `f_lstFree` (syntactic difference);
- `LIST_SUM_LOOP` instead of `LIST_FREE_LOOP` (syntactic difference); and
- `LIST_SUM_BODY` instead of `LIST_FREE_BODY` (syntactic difference).

This leads us to believe that the most part of `Z`- and `REC`-related
invariants could be synthesised automatically, with the user only having
to describe the function behavior manually.

## A simple list-length client

To demonstrate basic client reasoning, we prove correctness of a
[function](verification.md#simple-list-length-client)
which takes two lists of integers and returns the sum of their lengths:

```k
  claim
    <k>
      [ TWO_LISTS_LEN_SUM (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con integer length(XS) +Int length(YS) > ...
    </k>
    <env> _ => .Map </env>
```

```k
endmodule
```