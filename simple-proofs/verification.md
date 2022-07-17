# Example list algorithms: implementation

```k
requires "uplc.md"
```

## Recursive combinators

```k
module RECURSIVE-COMBINATORS
  imports UPLC-SYNTAX
```

### The standard Z combinator

```k
  syntax Term ::= "Z" [alias]
  rule Z => (lam f_0
              [ (lam s_0 [ f_0 (lam x_0 [ s_0 s_0 x_0 ]) ])
                (lam s_0 [ f_0 (lam x_0 [ s_0 s_0 x_0 ]) ]) ])
```



### Variation of the Z combinator used in the Plutus pipeline

```k
  syntax Term ::= "REC" [alias]
  rule REC => (lam f_0
                [ (lam s_0 [ s_0 s_0 ])
                  (lam s_0 (lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ])) ] )
```

### Helpful shorthand

For presentational reasons and easier debugging, we define the
following aliases that we use in the correctness proofs:

```k
  syntax Term ::= "Z_BODY" [alias]
  rule Z_BODY => (lam s_0 [ f_0 Z_CORE ])

  syntax Term ::= "Z_CORE" [alias]
  rule Z_CORE => (lam x_0 [ s_0 s_0 x_0 ])

  syntax Term ::= "Z_BODY_V" Map [alias]
  rule Z_BODY_V RHO => < lam s_0 [ f_0 Z_CORE ] RHO >

  syntax Term ::= "Z_CORE_V" Map [alias]
  rule Z_CORE_V RHO => < lam x_0 [ s_0 s_0 x_0 ] RHO >

  syntax Term ::= "REC_BODY" [alias]
  rule REC_BODY => (lam s_0 REC_CORE)

  syntax Term ::= "REC_CORE" [alias]
  rule REC_CORE => (lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ])

  syntax Term ::= "REC_BODY_V" Map [alias]
  rule REC_BODY_V RHO => < lam s_0 REC_CORE RHO >

  syntax Term ::= "REC_CORE_V" Map [alias]
  rule REC_CORE_V RHO => < lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ] RHO >

endmodule
```

## List algorithms

```k
module LIST-ALGORITHMS-SYNTAX
  import RECURSIVE-COMBINATORS
```

### List-free

The first algorithm we address is the list-free algorithm, `listFree(XS)`,
which frees a given mathematical list, `XS`. This algorithm is normally
used with linked lists stored in memory, freeing this memory, but in the
UPLC setting, it only removes the list elements one-by-one.

We encode the function body using three separate terms: one
for the entire implementation (`LIST_FREE`); one for the body of the function
(`LIST_FREE_BODY`), and one for the main loop (`LIST_FREE_LOOP`):

```
  --LIST_FREE----------- [
  |                        Z (lam f_lstSum
  | --LIST_FREE_BODY-------- (lam in_lst
  |  | --LIST_FREE_LOOP------- (force [ (force (builtin ifThenElse))
  |  |  |                        [ (force (builtin nullList)) in_lst ]
  |  |  |                        ( delay in_lst )
  |  |  |                        ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
  |  | ----------------------- ])
  |  ----------------------- )
  |                        )
  ---------------------- ]
```

This is a pattern that we will be following throughout---it is simply useful
shorthand, in that the invariant will always be stated on the main loop only,
and the body of the algorithm will be present in the environment of the invariant.

```k
  syntax Term ::= "LIST_FREE" [alias]
    rule LIST_FREE => (lam f_lstFree LIST_FREE_BODY)

  syntax Term ::= "LIST_FREE_BODY" [alias]
  rule LIST_FREE_BODY => (lam in_lst LIST_FREE_LOOP)

  syntax Term ::= "LIST_FREE_LOOP" [alias]
  rule LIST_FREE_LOOP =>
    (force
      [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_lst ]
        ( delay in_lst )
        ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
      ]
    )
```

We provide two recursive implementations of the algorithm, one using
the `Z` combinator (`Z_LIST_FREE`) and one using the `REC` combinator
(`REC_LIST_FREE`):

```k
  syntax Term ::= "LIST_FREE_Z" [alias]
    rule LIST_FREE_Z => [ Z LIST_FREE ]

  syntax Term ::= "LIST_FREE_REC" [alias]
    rule LIST_FREE_REC => [ REC LIST_FREE ]
```

### List-sum

The next algorithm that we implement is the list-sum algorithm,
`listSum(XS)`, which returns the sum of all of the values provided in
the list of integers `XS`. The implementation of the algorithm
is as follows, and we again split it into three terms, as for the list-free
algorithm:

```
  --LIST_SUM------------ [
  |                        Z (lam f_lstSum
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
  ---------------------- ]
```

```k
  syntax Term ::= "LIST_SUM" [alias]
    rule LIST_SUM => (lam f_lstSum LIST_SUM_BODY)

  syntax Term ::= "LIST_SUM_BODY" [alias]
    rule LIST_SUM_BODY => (lam in_lst LIST_SUM_LOOP)

  syntax Term ::= "LIST_SUM_LOOP" [alias]
    rule LIST_SUM_LOOP =>
      (force [ (force (builtin ifThenElse))
          [ (force (builtin nullList)) in_lst ]
          ( delay (con integer 0) )
          ( delay
            [ (builtin addInteger)
              [ (force (builtin headList)) in_lst ]
              [ f_lstSum [ (force (builtin tailList)) in_lst ] ]
            ]
          )
        ]
      )
```

and again implement recursion on top using both the `Z` and `REC` combinators:

```k
  syntax Term ::= "LIST_SUM_Z" [alias]
    rule LIST_SUM_Z => [ Z LIST_SUM ]

  syntax Term ::= "LIST_SUM_REC" [alias]
    rule LIST_SUM_REC => [ REC LIST_SUM ]
```

### List-length

The next algorithm that we consider is the list-length algorithm,
`listSum(XS)`, which returns the length of the list `XS`. The
implementation of the algorithm is as follows, and we encode it split
into three terms, as for the previous two algorithms:

```
  --LIST_LEN------------ [
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
  ---------------------- ]
```

```k
  syntax Term ::= "LIST_LEN" [alias]
  rule LIST_LEN => (lam f_lstLen LIST_LEN_BODY)

  syntax Term ::= "LIST_LEN_BODY" [alias]
  rule LIST_LEN_BODY => (lam in_lst LIST_LEN_LOOP)

  syntax Term ::= "LIST_LEN_LOOP" [alias]
  rule LIST_LEN_LOOP =>
  (force
    [ (force (builtin ifThenElse))
      [ (force (builtin nullList)) in_lst ]
      ( delay (con integer 0) )
      ( delay
        [ (builtin addInteger)
          (con integer 1)
          [ f_lstLen [ (force (builtin tailList)) in_lst ] ]
        ]
      )
    ]
  )
```

and again implement recursion on top using both the `Z` and `REC` combinators:

```k
  syntax Term ::= "LIST_LEN_Z" [alias]
  rule LIST_LEN_Z => [ Z LIST_LEN ]

  syntax Term ::= "LIST_LEN_REC" [alias]
  rule LIST_LEN_REC => [ REC LIST_LEN ]
```

#### Simple list-length client

We also implement a simple client of the list-length algorithm, which
takes two lists and returns the sum of their lengths, deliberately
calculating the two lengths using the two different implementations:

```k
  syntax Term ::= "TWO_LISTS_LEN_SUM" [alias]
    rule TWO_LISTS_LEN_SUM =>
    (lam in_lst1
      (lam in_lst2
        [ (builtin addInteger)
          [ LIST_LEN_Z   in_lst1 ]
          [ LIST_LEN_REC in_lst2 ]
        ]
      )
    )
```

### List-longer

Finally, to demonstrate what invariants look like for functions with multiple parameters,
we implement, specify, and verify the `listLonger(XS, YS)` algorithm, which takes two lists,
`XS` and `YS`, and returns `True` if `XS` is longer than `YS`, and `False` otherwise. Given
our goal, we implement this algorithm using a loop rather than using list-length:

```
  --LIST_LONGER---------- [
  |                         Z (lam f_lstLen
  | --LIST_LONGER_BODY------- (lam in_lst1
  |  |  |                       (lam in_lst2
  |  | --LIST_LONGER_LOOP-------- (force [ (force (builtin ifThenElse))
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

```k
  syntax Term ::= "LIST_LONGER" [alias]
    rule LIST_LONGER => (lam f_lstLonger LIST_LONGER_BODY)

  syntax Term ::= "LIST_LONGER_BODY" [alias]
  rule LIST_LONGER_BODY =>
    (lam in_lst1
      (lam in_lst2
        LIST_LONGER_LOOP
      )
    )

  syntax Term ::= "LIST_LONGER_LOOP" [alias]
  rule LIST_LONGER_LOOP =>
    (force
      [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_lst1 ]
        ( delay (con bool False) )
        ( delay
          ( force
            [ (force (builtin ifThenElse))
              [ (force (builtin nullList)) in_lst2 ]
              ( delay (con bool True) )
              ( delay [ f_lstLonger
                          [ (force (builtin tailList)) in_lst1 ]
                          [ (force (builtin tailList)) in_lst2 ] ] )
            ]
          )
        )
      ]
    )
```

and again implement recursion on top using both the `Z` and `REC` combinators:

```k
  syntax Term ::= "LIST_LONGER_Z" [alias]
    rule LIST_LONGER_Z => [ Z (lam f_lstLonger LIST_LONGER_BODY) ]

  syntax Term ::= "LIST_LONGER_REC" [alias]
    rule LIST_LONGER_REC => [ REC (lam f_lstLonger LIST_LONGER_BODY) ]

endmodule
```

## Auxiliary functions and predicates

```k
module AUXILIARIES
  imports UPLC-SYNTAX
```

### `allInts(XS)`: capturing that `XS` is a list of integers

```k
  syntax Bool ::= allInts(ConstantList) [function, functional]

  rule allInts(              .ConstantList) => true
  rule allInts(C:Constant, XS:ConstantList) => isInt(C) andBool allInts(XS)
```

### `sum(XS)`: calculating the sum of a given list of integers `XS`

```k
  syntax Int ::= sum(ConstantList) [function]

  rule sum(         .ConstantList ) => 0              [simplification]
  rule sum(I:Int, XS:ConstantList ) => I +Int sum(XS) [simplification]

  rule #Ceil ( sum ( XS ) ) => { true #Equals allInts(XS) } [simplification]
```

### `length(XS)`: calculating the length of a given list `XS`

```k
  syntax Int ::= length(ConstantList) [function, functional]

  rule length(     .ConstantList ) => 0
  rule length(_, XS:ConstantList ) => 1 +Int length(XS)
```

### `longerList(XS, YS)`: capturing that the list `XS` is longer than the list `YS`

```k
  syntax Constant ::= longer(ConstantList, ConstantList) [function, functional]

  rule longer(.ConstantList, _) => False
  rule longer(_, .ConstantList) => True
  rule longer((_, XS:ConstantList), (_, YS:ConstantList)) => longer(XS, YS)

endmodule
```

## Additional Simplifications

```k
module SIMPLIFICATIONS
  imports BOOL
  imports K-EQUAL
  imports MAP-SYMBOLIC
```

### Map Reasoning

```k
  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification]

  rule
  { M1:Map [ K1 <- V1 ] [ K2 <- V2 ] #Equals
    M2:Map [ K1 <- V3 ] [ K2 <- V4 ] } =>
  {
    true #Equals
            V1 ==K V3 andBool V2 ==K V4
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ]
  } [simplification]

  rule
  { M1:Map [ K1 <- V1 ] [ K2 <- V2 ] [ K3 <- V3 ] #Equals
    M2:Map [ K1 <- V4 ] [ K2 <- V5 ] [ K3 <- V6 ] } =>
  {
    true #Equals
            V1 ==K V4 andBool V2 ==K V5 andBool V3 ==K V6
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ]
  } [simplification]

endmodule
```

## Main module

```k
module VERIFICATION
  imports AUXILIARIES
  imports LIST-ALGORITHMS-SYNTAX
  imports SIMPLIFICATIONS
  imports UPLC

endmodule
```