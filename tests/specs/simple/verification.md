# Example list algorithms: implementation

```k
requires "../verification-common.md"
requires "uplc-rw-helpers.md"
requires "uplc-genvironment-instance.md"
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
                  (lam s_0 (lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ])) ])
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
  --LIST_FREE----------- (lam f_lstFree
  | --LIST_FREE_BODY------ (lam in_lst
  | | --LIST_FREE_LOOP------ (force [ (force (builtin ifThenElse))
  | | |                        [ (force (builtin nullList)) in_lst ]
  | | |                        ( delay in_lst )
  | | |                        ( delay [
  | | |                                  f_lstFree
  | | |                                  [ (force (builtin tailList)) in_lst ]
  | | |                                ] )
  | | ---------------------- ])
  | ---------------------- )
  ---------------------- )
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
  --LIST_SUM------------ (lam f_lstSum
  | --LIST_SUM_BODY------- (lam in_lst
  |  | --LIST_SUM_LOOP------ (force [ (force (builtin ifThenElse))
  |  |  |                      [ (force (builtin nullList)) in_lst ]
  |  |  |                      ( delay (con integer 0) )
  |  |  |                      ( delay
  |  |  |                        [ (builtin addInteger)
  |  |  |                          [ (force (builtin headList)) in_lst ]
  |  |  |                          [ f_lstSum [ (force (builtin tailList)) in_lst ] ]
  |  |  |                        ]
  |  |  |                      )
  |  | --------------------- ])
  |  --------------------- )
  ---------------------- )
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
  --LIST_LEN------------ (lam f_lstLen
  | --LIST_LEN_BODY-------- (lam in_lst
  |  | --LIST_LEN_LOOP------- (force [ (force (builtin ifThenElse))
  |  |  |                       [ (force (builtin nullList)) in_lst ]
  |  |  |                       ( delay (con integer 0) )
  |  |  |                       ( delay
  |  |  |                         [ (builtin addInteger)
  |  |  |                           (con integer 1)
  |  |  |                           [ f_lstLen [ (force (builtin tailList)) in_lst ] ]
  |  |  |                         ]
  |  |  |                       )
  |  | ---------------------- ])
  |  ---------------------- )
  ---------------------- )
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

#### Tail-recursive list-length

We also implement a tail-recursive version of list-length, as follows:

```k
  syntax Term ::= "LIST_LEN_TAIL_REC" [alias]
  rule LIST_LEN_TAIL_REC => [ REC LIST_LEN_TAIL ]

  syntax Term ::= "LIST_LEN_TAIL" [alias]
  rule LIST_LEN_TAIL => (lam f_lstLenTail LIST_LEN_TAIL_BODY)

  syntax Term ::= "LIST_LEN_TAIL_BODY" [alias]
  rule LIST_LEN_TAIL_BODY => (lam ac_0 (lam rest_0 LIST_LEN_TAIL_LOOP))

  syntax Term ::= "LIST_LEN_TAIL_LOOP" [alias]
  rule LIST_LEN_TAIL_LOOP =>
  (force
    [ (force (builtin ifThenElse))
      [ (force (builtin nullList)) rest_0 ]
      ( delay ac_0 )
      ( delay
          [ f_lstLenTail
              [
                (builtin addInteger)
                ac_0
                (con integer 1)
              ]
              [ (force (builtin tailList)) rest_0 ]
          ]
      )
    ]
  )
```

### List-max

```k
  syntax Term ::= "LIST_MAX" [alias]
  rule LIST_MAX => (lam lst_0 [ LIST_MAX_TAIL_REC [ (force (builtin headList)) lst_0 ] [ (force (builtin tailList)) lst_0 ] ])

  syntax Term ::= "LIST_MAX_TAIL_REC" [alias]
  rule LIST_MAX_TAIL_REC => [ REC LIST_MAX_TAIL ]

  syntax Term ::= "LIST_MAX_TAIL" [alias]
  rule LIST_MAX_TAIL => (lam f_lstMaxTail LIST_MAX_TAIL_BODY)

  syntax Term ::= "LIST_MAX_TAIL_BODY" [alias]
  rule LIST_MAX_TAIL_BODY => (lam ac_0 (lam rest_0 LIST_MAX_TAIL_LOOP))

  syntax Term ::= "LIST_MAX_TAIL_LOOP" [alias]
  rule LIST_MAX_TAIL_LOOP =>
  (force
    [ (force (force (builtin chooseList)))
      rest_0
      ( delay ac_0 )
      ( delay
          [ f_lstMaxTail
              [ ( force ( builtin ifThenElse ) )
                [ ( builtin lessThanInteger ) ac_0 [ ( force ( builtin headList ) ) rest_0 ] ]
                [ ( force ( builtin headList ) ) rest_0 ]
                ac_0
              ]
              [ (force (builtin tailList)) rest_0 ]
          ]
      )
    ]
  )
```

### List-longer

Finally, to demonstrate what invariants look like for functions with multiple parameters,
we implement, specify, and verify the `listLonger(XS, YS)` algorithm, which takes two lists,
`XS` and `YS`, and returns `True` if `XS` is longer than `YS`, and `False` otherwise. Given
our goal, we implement this algorithm using a loop rather than using list-length:

```
  --LIST_LONGER---------- (lam f_lstLonger
  | --LIST_LONGER_BODY----- (lam in_lst1
  |  |  |                      (lam in_lst2
  |  | --LIST_LONGER_LOOP-------- (force [ (force (builtin ifThenElse))
  |  |  |                           [ (force (builtin nullList)) in_lst ]
  |  |  |                           ( delay (con integer 0) )
  |  |  |                           ( delay
  |  |  |                             [ (builtin addInteger)
  |  |  |                               (con integer 1)
  |  |  |                               [ f_lstLonger [ (force (builtin tailList)) in_lst ] ]
  |  |  |                             ]
  |  |  |                           )
  |  | -------------------------- ])
  |  |                          )
  |  ------------------------ )
  |                         )
  ----------------------- )
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
  syntax Bool ::= allInts(ConstantList) [function, functional, no-evaluators]

  rule { true #Equals allInts(              .ConstantList) } => #Top [simplification]
  rule { true #Equals allInts(C:Constant, XS:ConstantList) } => #Exists I:Int. { C #Equals I } #And { true #Equals allInts(XS) }
    [simplification, unboundVariables(I)]
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

### `#max(X, Y)` and `#maxList(XS)`: calculating the max element of a given list `XS`

```k
  syntax Int ::= #max( Int, Int ) [function, functional]
  rule #max( X, Y ) => X requires notBool X <Int Y
  rule #max( X, Y ) => Y requires         X <Int Y

  syntax Int ::= #maxList(ConstantList) [function]
  rule #maxList(X:Int, .ConstantList)   => X
  rule #maxList(X:Int, XS:ConstantList) => #max( X, #maxList( XS ) )
    requires notBool XS ==K .ConstantList

  rule #Ceil( #maxList( XS ) )
    => { true #Equals allInts(XS) } #And #Not ( { XS #Equals .ConstantList } ) [simplification]

  rule { #max( Y, #maxList( X, XS ) ) #Equals #maxList( X, XS ) } => #Top
    requires         Y <Int X [simplification]

  rule { #max( Y, #maxList( X, XS ) ) #Equals #maxList( Y, XS ) } => #Top
    requires notBool Y <Int X [simplification]
```

### `longerList(XS, YS)`: capturing that the list `XS` is longer than the list `YS`

```k
  syntax Constant ::= longer(ConstantList, ConstantList) [function, functional]

  rule longer(.ConstantList, _) => False
  rule longer(_, .ConstantList) => True
  rule longer((_, XS:ConstantList), (_, YS:ConstantList)) => longer(XS, YS)

endmodule
```

## Trivial Policies

```k
module POLICIES
  imports UPLC-RW-HELPERS
  syntax Term ::= "TRIVIAL_POLICY_TRUE" [macro]
  rule TRIVIAL_POLICY_TRUE =>
    (lam r_0
      (lam p_0
        [
          (lam tup_0
            [
              (lam t_0
                (force
                  [
                    [
                      (force
                        [
                          bool_match
                          [
                            [
                              (lam ds_0
                                (lam ds_1
                                  [
                                    (force [ unit_match ds_0 ])
                                    true_id
                                  ]
                                )
                              )
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    [
                                      (force (force (builtin fstPair)))
                                      [ (builtin unConstrData) r_0 ]
                                    ]
                                    (con integer 0)
                                  ]
                                  (lam ds_0 unit_id)
                                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                                ]
                                unitval_id
                              ]
                            ]
                            [
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    [ (force (force (builtin fstPair))) (force tup_0) ]
                                    (con integer 0)
                                  ]
                                  (lam ds_0
                                    [
                                      scriptContext_id
                                      [
                                        fUnsafeFromDataTxInfo_cunsafeFromBuiltinData
                                        [ (force (builtin headList)) (force t_0) ]
                                      ]
                                      [
                                        fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData
                                        [
                                          (force (builtin headList))
                                          [ (force (builtin tailList)) (force t_0) ]
                                        ]
                                      ]
                                    ]
                                  )
                                ]
                                [ THROW_ERROR_LAM reconstructCaseError_id ]
                              ]
                              unitval_id
                            ]
                          ]
                        ]
                      )
                      (delay unit_id)
                    ]
                    [ THROW_ERROR_DELAY (con string "PT5" ) ]
                  ]
                )
              )
              (delay [ (force (force (builtin sndPair))) (force tup_0) ])
            ]
          )
          (delay [ (builtin unConstrData) p_0 ])
        ]
      )
    )
syntax Term ::= "TRIVIAL_POLICY_FALSE" [macro]
  rule TRIVIAL_POLICY_FALSE =>
    (lam r_0
      (lam p_0
        [
          (lam tup_0
            [
              (lam t_0
                (force
                  [
                    [
                      (force
                        [
                          bool_match
                          [
                            [
                              (lam ds_0
                                (lam ds_1
                                  [
                                    (force [ unit_match ds_0 ])
                                    false_id
                                  ]
                                )
                              )
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    [
                                      (force (force (builtin fstPair)))
                                      [ (builtin unConstrData) r_0 ]
                                    ]
                                    (con integer 0)
                                  ]
                                  (lam ds_0 unit_id)
                                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                                ]
                                unitval_id
                              ]
                            ]
                            [
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    [ (force (force (builtin fstPair))) (force tup_0) ]
                                    (con integer 0)
                                  ]
                                  (lam ds_0
                                    [
                                      scriptContext_id
                                      [
                                        fUnsafeFromDataTxInfo_cunsafeFromBuiltinData
                                        [ (force (builtin headList)) (force t_0) ]
                                      ]
                                      [
                                        fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData
                                        [
                                          (force (builtin headList))
                                          [ (force (builtin tailList)) (force t_0) ]
                                        ]
                                      ]
                                    ]
                                  )
                                ]
                                [ THROW_ERROR_LAM reconstructCaseError_id ]
                              ]
                              unitval_id
                            ]
                          ]
                        ]
                      )
                      (delay unit_id)
                    ]
                    [ THROW_ERROR_DELAY (con string "PT5" ) ]
                  ]
                )
              )
              (delay [ (force (force (builtin sndPair))) (force tup_0) ])
            ]
          )
          (delay [ (builtin unConstrData) p_0 ])
        ]
      )
    )
endmodule
```

## Main module

```k
module VERIFICATION
  imports VERIFICATION-COMMON
  imports AUXILIARIES
  imports LIST-ALGORITHMS-SYNTAX
  imports POLICIES
  imports UPLC-GENVIRONMENT-INSTANCE

endmodule
```
