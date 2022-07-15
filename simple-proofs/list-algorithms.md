# List algorithms: implementation, loop invariants, correctness proofs

All of the algorithms are recursive and use the `Z` combinator to 
implement recursion. The `Z` combinator is defined in the standard way:

```
  syntax Term ::= "Z" [alias]
  rule Z => (lam f_0 [ZBody ZBody])
```

where `ZBody` is defined as 

```
  syntax Term ::= "ZBody" [alias]
  rule ZBody => (lam x_0 [f_0 (lam y_0 [x_0 x_0 y_0])])
```

## List-Free

The first algorithm we address is the list-free algorithm, `listFree(XS)`, 
which frees a given mathematical list, `XS`. This algorithm is normally 
used with (singly-linked) lists that are in memory, freeing this memory,
but in the UPLC setting, it only removes the list elements one-by-one.

The implementation of the list-free algorithm is as follows:

```
  [ 
    Z (lam f_lstFree 
        (lam in_lst 
          (force 
            [ (force (builtin ifThenElse))
              [ (force (builtin nullList)) in_lst ]
              ( delay in_lst )
              ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
            ]
          )
        )
      ) 
  ]
```

and what we do is encode it as three separate terms, one for
the entire implementation:

```
  syntax Term ::= "LIST_FREE" [alias]
    rule LIST_FREE =>
      [ Z (lam f_lstFree LIST_FREE_BODY) ]
```

one for the body of the function:

```
  syntax Term ::= "LIST_FREE_BODY" [alias]
  rule LIST_FREE_BODY => 
    (lam in_lst 
        LIST_FREE_LOOP_BODY
      )
```

and one for the body of the main loop:

```
  syntax Term ::= "LIST_FREE_LOOP_BODY" [alias]
  rule LIST_FREE_LOOP_BODY => 
    (force 
      [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_lst ]
        ( delay in_lst )
        ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
      ]
    )
```

This is a pattern that we will be following throughout - it is simply useful
shorthand, in that the invariant will always be stated on the main loop 
only, and the body of the algorithm will be present in the environment of 
the invariant.

The correctness claim for the entire list-free (together with the preamble),
as this is the first algorithm of this file, is stated as follows:

```k
requires "verification.k"

module LIST-ALGORITHMS
  imports VERIFICATION

  claim 
    <k>
      [ LIST_FREE ( con list(integer) [ _XS:ConstantList ] ) ]
        => 
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env> _ => .Map </env>
```

and what it says is that the execution of the UPLC term 
`[ LIST_FREE ( con list(integer) [ _XS:ConstantList ] ) ]`,
starting from an arbitrary environment (denoted by `_`), 
results in the UPLC term `< con list(integer) [ .ConstantList ] >`
and an empty environment (denoted by `.Map`)---in other words,
that the list-free algorithm consumes the given list.

In order to prove this claim, we have to prove the appropriate
loop invariant, which `K` can then re-use it in the proof of
the full algorithm. It is stated as follows:

```k
  claim 
    <k>
      LIST_FREE_LOOP_BODY
        =>
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstFree <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ _XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
```

and states that the execution of the body of the loop 
(`LIST_FREE_LOOP_BODY`) in the given initial environment (discussed shortly)
terminates, resulting in the empty list and the empty environment.

This invariant reveals a pattern that can be re-used for any other recursive
function implemented using the `Z` combinator. First, the starting term in 
the invariant is always the loop body, and the final term is the result of
executing *the entire loop*, not just one of its iterations. 

The initial environment has a structure specific to the `Z` combinator, whereas
the final environment is always the empty environment. In particular, the initial 
one extends an arbitrary environment (`RHO`) with bindings for the function itself 
(`f_lstFree`) and the passed-in parameters (in this case, `in_lst`, cf. 
the [list-longer](#list-longer) algorithm for an example with multiple parameters). 
While the latter are straightforward, the former is more intricate, but always 
follows the same structure:

```
  < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] >
```

where `RHO_1` is shorthand for

```
  RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
```

in which `f_0` is bound to the body of the list-free function, to be 
evaluated in the environment `RHO`.

## List-Sum

The next algorithm that we consider is the list-sum algorithm, 
`listSum(IS)`, which returns the sum of all of the values 
provided in the list of integers `IS`. The implementation of the algorithm
is as follows, and encode it split into three terms, as for the list-free 
algorithm:

```
  --LIST_SUM------------- [ 
  |                        Z (lam f_lstSum       
  | --LIST_SUM_BODY--------- (lam in_lst 
  |  | --...LOOP_BODY--------- (force [ (force (builtin ifThenElse))
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

To express correctness of list-sum, we also have to define a predicate 
`allInts(IS)`, which checks that a given list `IS` is a list of integers:

```
  syntax Bool ::= allInts(ConstantList) [function, functional]

  rule allInts(              .ConstantList) => true                              
  rule allInts(C:Constant, IS:ConstantList) => isInt(C) andBool allInts(IS) 
```

and an auxiliary function, `sum(XS)`, which sums a given list of integers:

```
  syntax Int ::= sum(ConstantList) [function]

  rule sum(         .ConstantList ) => 0              [simplification]
  rule sum(I:Int, IS:ConstantList ) => I +Int sum(IS) [simplification]
```

With these in place, the correctness claim for the list-sum
algorithm is formulated as follows:

```k
  claim <k>
    [ LIST_SUM ( con list(integer) [ XS:ConstantList ] ) ] 
      => 
    < con integer sum(XS) >
    ... </k>
    <env> _ => .Map </env>
    requires allInts(XS)
```

stating that the list-sum algorithm terminates, returning the 
sum of the passed-in list. As for list-free, the initial environment
is arbitrary. and the final environment is empty. In addition, 
we have to state that the passed-in list, `XS`, has to be a list
of integer, using `requires allInts(XS)`.

The required loop invariant is formulated as follows:

```k
  claim 
    <k>
      LIST_SUM_LOOP_BODY
        =>
      < con integer sum(XS) > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstSum <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstSum LIST_SUM_BODY RHO > ]
      andBool allInts(XS)
```

and comparing it to the list-free invariant, we can see that the only differences are: 

- `< con integer sum(XS) >` instead of `< con list(integer) [ .ConstantList ] >` (semantic);
- `f_lstSum` instead of `f_lstFree` (syntactic);
- `LIST_SUM_LOOP_BODY` instead of `LIST_FREE_LOOP_BODY` (syntactic); and
- `LIST_SUM_BODY` instead of `LIST_FREE_BODY` (syntactic).

## List-length

The next algorithm that we consider is the list-length algorithm, 
`listSum(XS)`, which returns the length of the list `XS`. The 
implementation of the algorithm is as follows, and encode it split 
into three terms, as for the previous two algorithm:

```
  --LIST_SUM------------- [ 
  |                        Z (lam f_lstLen       
  | --LIST_SUM_BODY--------- (lam in_lst 
  |  | --...LOOP_BODY--------- (force [ (force (builtin ifThenElse))
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
length of the passed-in list. As for the previous two algorithms, 
the initial environment is arbitrary. and the final environment 
is empty.

The required loop invariant is formulated as follows:

```k
  claim 
    <k>
      LIST_LEN_LOOP_BODY
        =>
      < con integer length(XS) > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstLen <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]
```

and comparing it to the list-sum and list-free invariants, we can see the same differences, one semantic (regarding the result)
and three syntactic ones (regarding different term names).

### A simple list-length client

We also implement a simple client of the list-length algorithm, which 
takes two lists and returns the sum of their lengths:

```
  syntax Term ::= "TWO_LISTS_LEN_SUM" [alias]
    rule TWO_LISTS_LEN_SUM => 
    (lam in_lst1
      (lam in_lst2
        [ (builtin addInteger)
          [ LIST_LEN in_lst1 ] 
          [ LIST_LEN in_lst2 ] 
        ]
      )
    )
```

and verify its correctness:

```k
  claim 
    <k>
      [ TWO_LISTS_LEN_SUM (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con integer length(XS) +Int length(YS) > ... 
    </k>
    <env> _ => .Map </env>
```

## List-longer

Finally, to demonstrate what loop invariants look like for functions with multiple parameters,
we implement, specify, and verify the `listLonger(XS, YS)` algorithm, which takes two lists,
`XS` and `YS`, and returns `True` if `XS` is longer than `YS`, and `False` otherwise. Given
our goal, we implement this algorithm using a loop rather than using list-length:

```
  --LIST_LONGER---------- [ 
  |                         Z (lam f_lstLen       
  | --LIST_LONGER_BODY------- (lam in_lst1
  |  |  |                       (lam in_lst2 
  |  | --...LOOP_BODY------------ (force [ (force (builtin ifThenElse))
  |  |  |                           [ (force (builtin nullList)) in_lst ]
  |  |  |                           ( delay (con integer 0) )
  |  |  |                           ( delay
  |  |  |                             [ (builtin addInteger)
  |  |  |                               (con integer 1)
  |  |  |                               [ f_lstLen [ (force (builtin tailList)) in_lst ] ]
  |  |  |                             ]
  |  |  |                           )
  |  | -------------------------- ])
  |  |                          )
  |  ------------------------ )
  |                         ) 
  ----------------------- ]
```

For this algorithm, we have to define a predicate that 
captures its behaviour,
`listLonger(XS, YS)`:

```
  syntax Constant ::= longerList(ConstantList, ConstantList) [function]

  rule longerList(.ConstantList, _) => False                                        [simplification]
  rule longerList(_, .ConstantList) => True                                         [simplification]
  rule longerList((_, XS:ConstantList), (_, YS:ConstantList)) => longerList(XS, YS) [simplification]
```

With this predicate in place, the correctness claim for the list-longer
algorithm is given as follows:

```k
  claim 
    <k>
      [ LIST_LONGER (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con bool longerList(XS, YS) >
    ... </k>
    <env> _ => .Map </env>
```

and the correspondig loop invariant (together with the module epilogue) is as follows:

```k
  claim 
  <k>
    LIST_LONGER_LOOP_BODY
      =>
    < con bool longerList(XS, YS) > ...
  </k>
  <env> RHO:Map 
          [ f_lstLonger <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
          [ in_lst1 <- < con list ( integer ) [ XS:ConstantList ] > ] 
          [ in_lst2 <- < con list ( integer ) [ YS:ConstantList ] > ] => .Map 
  </env>
  requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO > ]

endmodule
```

Comparing this invariant to the previous ones, we can see the the only conceptual difference is that both of the 
function parameters are in the initial environment. This can be generalised to multi-parameter functions in that all of
the function parameters need to be in the initial environment with the appropriate bindings.