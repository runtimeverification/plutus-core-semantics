# List algorithms: implementation, loop invariants, correctness proofs

All of the algorithms are recursive and use the `REC` combinator used
as part of the Plutus compilation chain, to implement recursion. 
The `REC` combinator is defined as follows:

```
  syntax Term ::= "REC" [alias]
  rule REC => 
    (lam f_0 [ 
      (force (delay (lam s_0 [ s_0 s_0 ])))
          (lam s_0 (lam x_0 [ [ f_0 [ (force (delay (lam s_0 [ s_0 s_0 ]))) s_0 ] ] x_0 ])) ] )
```

## List-Sum

The first algorithm we address is the list-sum algorithm, `listSum(IS)`, 
which which returns the sum of all of the values provided in the list of 
integers `IS`. The implementation of the list-sum algorithm is as follows:

```
  --LIST_SUM------------- [ 
  |                        REC (lam f_lstSum       
  | --LIST_SUM_BODY--------- (lam in_lst 
  |  | --LIST_SUM_LOOP-------- (force [ (force (builtin ifThenElse))
  |  |  |                        [ (force (builtin nullList)) in_lst ]
  |  |  |                        ( delay (con integer 0) )
  |  |  |                        ( delay
  |  |  |                          [ (builtin addInteger)
  |  |  |                            [ (force (builtin headList)) in_lst ]
  |  |  |                            [ f_lstSum [ (force (builtin tailList)) in_lst ] ]
  |  |  |                          ]
  |  |  |                        )
  |  | ----------------------- ])
  |  ----------------------- )
  |                        ) 
  ----------------------- ]
```

and what we do is encode it as three separate terms:
- one for the entire implementation, `LIST_SUM`;
- one for the body of the function, `LIST_SUM_BODY`; and
- one for the body of the main loop, `LIST_SUM_LOOP`.

This is a pattern that we will be following throughout - it is simply useful
shorthand, in that the invariant will always be stated on the main loop 
only, and the body of the algorithm will be present in the environment of 
the invariant.

The correctness claim for the entire list-sum function (together with the module 
preamble, as this is the first claim of this file), is stated as follows:

```k
requires "list-algorithms.k"

module LIST-ALGORITHMS-CLAIMS
  imports LIST-ALGORITHMS

  claim <k>
    [ LIST_SUM ( con list(integer) [ IS:ConstantList ] ) ] 
      => 
    < con integer sum(IS) >
    ... </k>
    <env> _ => .Map </env>
    requires allInts(IS)
```

and what it says is that the execution of the UPLC term 
`[ LIST_SUM ( con list(integer) [ IS:ConstantList ] ) ] `,
starting from an arbitrary environment (denoted by `_`), 
results in the UPLC term `< con integer sum(IS) >`
and an empty environment (denoted by `.Map`)---in other words,
that the list-sum algorithm returns the sum of the given list.

In order to prove this claim, we have to prove the appropriate
loop invariant, which `K` can then re-use it in the proof of
the full algorithm. It is stated as follows:

```k
  claim 
    <k>
      LIST_SUM_LOOP
        =>
      < con integer sum(IS) > ... 
    </k>
    <env>
         RHO 
          [ f_lstSum <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO_1 [ s_0 <- < lam s_0 ( lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] ) RHO_1 > ] > ] 
          [ in_lst <- < con list ( integer ) [ IS ] > ] => .Map
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstSum LIST_SUM_BODY RHO > ]
      andBool allInts(IS)
```

and states that the execution of the body of the loop 
(`LIST_SUM_LOOP`) in the given initial environment (discussed shortly)
terminates, resulting in the sum of the entire list, `sum(IS)`, and 
the empty environment.

This invariant reveals a pattern that can be re-used for any other recursive
function implemented using the `REC` combinator. First, the starting term in 
the invariant is always the loop body, and the final term is the result of
executing *the entire loop*, not just one of its iterations. 

The initial environment has a structure specific to the `REC` combinator, whereas
the final environment is always the empty environment. In particular, the initial 
one extends an arbitrary environment (`RHO`) with bindings for the function itself 
(`f_lstSum`) and the passed-in parameters (in this case, `in_lst`, any additional
parameters would follow similarly). While the latter are straightforward, the former 
is more intricate, but always follows the same structure:

```
  < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO_1 [ s_0 <- < lam s_0 ( lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] ) RHO_1 > ] >
```

where `RHO_1` is shorthand for

```
  RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
```

in which `f_0` is bound to the body of the list-free function, to be 
evaluated in the environment `RHO`.

## List-length

The next algorithm that we consider is the list-length algorithm, 
`listSum(XS)`, which returns the length of the list `XS`. The 
implementation of the algorithm is as follows, and we encode it split 
into three terms, as for the list-sum algorithm:

```
  --LIST_LEN------------- [ 
  |                        Z (lam f_lstLen       
  | --LIST_LEN_BODY--------- (lam in_lst 
  |  | --LIST_LEN_LOOP-------- (force [ (force (builtin ifThenElse))
  |  |  |                        [ (force (builtin nullList)) in_lst ]
  |  |  |                        ( delay (con integer 0) )
  |  |  |                        ( delay
  |  |  |                          [ (builtin addInteger)
  |  |  |                            (con integer 1)
  |  |  |                            [ f_lstLen [ (force (builtin tailList)) in_lst ] ]
  |  |  |                          ]
  |  |  |                        )
  |  | ----------------------- ])
  |  ----------------------- )
  |                        ) 
  ----------------------- ]
```

To express correctness of list-length, similarly to the case of list-sum,
we have to define an auxiliary function that calculates the length of a given list, 
`length(XS)`:

```
  syntax Int ::= length(ConstantList) [function, functional]

  rule length(     .ConstantList) => 0
  rule length(_, IS:ConstantList) => 1 +Int length(IS)
```

With this in place, the correctness claim for the list-length
algorithm is formulated as follows:

```k
  claim 
    <k>
      [ LIST_LEN (con list(integer) [ XS:ConstantList ]) ] 
        => 
      < con integer length(XS) > ... 
    </k>
    <env> _ => .Map </env>
```

stating that the list-length algorithm terminates, returning the 
length of the passed-in list. As for the list-sum algorithm, 
the initial environment is arbitrary. and the final environment 
is empty.

The required loop invariant (together with the module epilogue, as 
this is the last claim of the file) is formulated as follows:

```k
  claim 
    <k>
      LIST_LEN_LOOP
        =>
      < con integer length(XS) > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstLen <- < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO_1 [ s_0 <- < lam s_0 ( lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] ) RHO_1 > ] > ]
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]

endmodule
```

and comparing it to the list-sum and list-free invariants, we can see the same differences, one semantic (regarding the result)
and three syntactic ones (regarding different term names).