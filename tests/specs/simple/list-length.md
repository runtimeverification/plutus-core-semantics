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
      .Map
        [ f_lstLen <- < lam x_0 [ s_0 s_0 x_0 ] .Map [ s_0 <- Z_BODY_V RHO ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstLen LIST_LEN_BODY .Map > ]

  claim
    <k>
      LIST_LEN_LOOP
        =>
      < con integer length(XS) > ...
    </k>
    <env>
      .Map
        [ f_lstLen <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO [ s_0 <- REC_BODY_V RHO ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstLen LIST_LEN_BODY .Map > ]
```

which, in comparison with the [list-free invariants](list-free.md#invariants),
exhibit only minor differences:
- `< con integer length(XS) >` instead of `< con list(integer) [ .ConstantList ] >`
   (semantic difference, as the two functions behave differently);
- `f_lstLen` instead of `f_lstFree` (syntactic difference);
- `LIST_LEN_LOOP` instead of `LIST_FREE_LOOP` (syntactic difference); and
- `LIST_LEN_BODY` instead of `LIST_FREE_BODY` (syntactic difference).

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

## Tail-recursive list-length

We also prove correctness of the tail-recursive list-length algorithm:

```k
  claim
    <k>
      [ LIST_LEN_TAIL_REC (con integer 0) (con list(integer) [ XS:ConstantList ]) ]
        =>
      < con integer length(XS) > ...
    </k>
    <env> _ => .Map </env>
```

using the appropriate invariant

```k
  claim
    <k>
      LIST_LEN_TAIL_LOOP
        =>
      < con integer AC +Int length(XS) > ...
    </k>
    <env>
      .Map
        [ f_lstLenTail <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO [ s_0 <- REC_BODY_V RHO ] > ]
        [ ac_0 <- < con integer AC:Int > ]
        [ rest_0 <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map
    </env>
    requires RHO ==K .Map [ f_0 <- < lam f_lstLenTail LIST_LEN_TAIL_BODY .Map > ]
```

```k
endmodule
```