# List-free: correctness claims

```k
requires "verification.md"

module LIST-FREE
  imports VERIFICATION
```

## Main claims

The correctness claims for the two recursive implementations of the list-free
function are as follows:

```k
  claim
    <k>
      [ LIST_FREE_Z (con list(integer) [ _XS:ConstantList ] ) ]
        =>
      < con list(integer) [ .ConstantList ] > ...
    </k>
    <env> _ => .Map </env>

  claim
    <k>
      [ LIST_FREE_REC (con list(integer) [ _XS:ConstantList ]) ]
        =>
      < con list(integer) [ .ConstantList ] > ...
    </k>
    <env> _ => .Map </env>
```

These claims state that the execution of the list-free function with
an arbitrary list of integers `_XS` passed in as parameter and starting
from an arbitrary environment (denoted by `_`) terminates, returning the
the empty list of integers (denoted by `.ConstantList`) and an empty
environment (denoted by `.Map`).

## Invariants

In order to prove these claims, we need to prove the corresponding
invariants, which `K` can then re-use in the proofs of the entire
functions. We start from the invariant for `LIST_FREE_Z`:

```k
  claim
    <k>
      LIST_FREE_LOOP
        =>
      < con list(integer) [ .ConstantList ] > ...
    </k>
    <env>
      RHO:Map
        [ f_lstFree <- < lam x_0 [ s_0 s_0 x_0 ] RHO_1 [ s_0 <- Z_BODY_V RHO_1 ] > ]
        [ in_lst <- < con list ( integer ) [ _XS:ConstantList ] > ] => .Map
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
```

which states that the execution of the list-free loop
(`LIST_FREE_LOOP`) in the given initial environment (discussed shortly)
terminates, resulting in the empty list and the empty environment.

This invariant reveals the pattern that can be re-used for any other recursive
function implemented using the `Z` combinator. In particular, the initial term
of the invariant is always the loop itself, and the final term is the result of
executing *the entire loop*, not just one of its iterations (this is the way in
which `K` handles loops).

The initial environment has a structure specific to the `Z` combinator
(cf. [the definition](verification.md/#recursive-combinators)
of the `Z` combinator)), whereas the final environment is *always* the
empty environment. In particular, the initial one extends an arbitrary
environment (`RHO`) with bindings for the function itself
(`f_lstFree`) and the passed-in parameters (in this case, `in_lst`).
While the latter part is straightforward, the former is more intricate, but always
follows the same structure:

```
  Z_CORE_V
    (RHO
      [ f_0 <- < lam F_ID F_BODY RHO > ]
      [ s_0 <- Z_BODY_V
                (RHO
                  [ f_0 <- < lam F_ID F_BODY RHO > ]
                )
      ]
    )
```

which denotes the core of the `Z` combinator evaluated in the environment
that extends the initial environment `RHO` with two bindings:
- one for `f_0`, which is bound to the evaluation of the considered function
  in the initial environment `RHO`;
- one for `s_0`, which is bound to the evaluation of the body of the `Z` combinator
  in the initial environment extended with the above binding for `f_0`.

The invariant for the `LIST_FREE_REC` implementation is as follows:

```k
  claim
    <k>
      LIST_FREE_LOOP
        =>
      < con list(integer) [ .ConstantList ] > ...
    </k>
    <env>
      RHO:Map
        [ f_lstFree <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO_1 [ s_0 <- REC_BODY_V RHO_1 ] > ]
        [ in_lst <- < con list ( integer ) [ _XS:ConstantList ] > ] => .Map
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
```

and differs from the one for `LIST_FREE_Z` *only* in the binding
for the function (`f_lstFree`) in the initial environment, which is expected
as we are using a different combinator to implement recursion. The remaining
parts---the initial term, final term, arbitrary environment, parameter bindings,
final environment, and auxiliary environment---are all the same. We believe
that this would be the only difference for all combinators that behave
equivalently to `Z` and `REC`.

```k
endmodule
```