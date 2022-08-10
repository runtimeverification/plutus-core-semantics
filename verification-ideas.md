# Datatype encodings

Consider the following datatype:

```
data T
  = C_0 T_0_0 ... T_0_M0
  | ...
  | C_N T_N_0 ... T_N_MN
```

Its compilation to uplc results in the following:
1. one identifier per constructor: these identifiers are
   used as some sort of selectors;
2. a matcher function, `T_match`, which I don't understand;
3. a decoder function, `fUnsafeFromDataT_cunsafeFromBuiltinData`,
   which takes uplc data and decodes it, assuming it represents `T`.

## General: Selectors and matcher

The per-constructor identifiers and the matcher function are represented as
follows:

```k
<<< some uplc code >>>
[
  // N-nested abstraction: one variable per constructor
  [
    (lam C_0
      ...
        (lam C_N
          (lam T_match
            << some uplc code >>
          )
        )
      ...
    )
  ]
  // N-application: one abstraction per constructor
  LAM_C_0_M0
  ...
  LAM_C_N_MN
]
<<< some uplc code >>>
```

where the terms `LAM_C_I_MI`, for `0 <= I <= N`, are of the following form:

```k
// MI-nested abstraction: one variable per argument of I-th constructor
(lam arg_0 ... (lam arg_MI
  // N-nested abstraction: one case variable per constructor
  (lam case_C_0
    ...
    (lam case_C_I
      // Relevant case variable applied to given arguments
      [ case_C_I arg_0 ... arg_MI ]
    )
  )
)
```

## General: Decoder

The decoder function is *roughly* of the following form:

```k
<<< some uplc code >>>
[
  (lam fUnsafeFromDataT_cunsafeFromBuiltinData
    <<< some uplc code >>>
  )
  // d: input data, which has to be constructor data (unConstrData)
  (lam d
    [
      // tup: a pair (cIdx:Int, cParams:list(data))
      (lam tup
        [
          // t: cParams
          (lam t
            [
              ...
               [
                  // The nested ts either isolate consecutive elements
                  // from cParams, or all equal cParams
                  (lam t
                    [
                      // index: cIdx
                      (lam index
                        CONSTRUCTOR_SWITCH
                      )
                      // cIdx
                      (delay [ (force (force (builtin fstPair))) (force tup) ])
                    ]
                  )
                  (delay [ (force (force (builtin sndPair))) (force t/tup) ])
                ]
              ...
            ]
          )
          // cParams
          (delay [ (force (force (builtin sndPair))) (force tup) ])
        ]
      )
      // (cIdx, cParams)
      (delay [ (builtin unConstrData) d ])
    ]
  )
]
```

where the `CONSTRUCTOR_SWITCH` term is an N-nested if-then-else, branching
on the constructor index `index (cIDx)` in reverse order, and decoding
and applying the parameters for each constructor appropriately:

```k
[
  [
    (force (builtin ifThenElse))
    [
      (builtin equalsInteger)
      (force index)
      (con integer N)
    ]
    // N-th constructor
    (lam ds
      [
        C_N
        << decoded arg_0 >>
        ...
        << decoded arg_MN >>
      ]
    )
    (lam ds
      [
        [
          (force (builtin ifThenElse))
          [
            (builtin equalsInteger)
            (force index)
            (con integer (N - 1))
          ]
          // N-1-st constructor
          (lam ds [ C_N-1 ... ] )
          (lam ds
            ...
              (lam ds
                [
                  [
                    (force (builtin ifThenElse))
                    [
                      (builtin equalsInteger)
                      (force index)
                      (con integer 0)
                    ]
                    // 0-th constructor
                    (lam ds [ C_0 ... ] )
                    // If the constructor index is not applicable,
                    // then the data was not encoded properly,
                    // and an error is thrown
                    [ THROW_ERROR_LAM reconstructCaseError ]
                  ]
                ]
                // This is effectively a force on the delay that is (lam ds ...
                unitval
              )
            ...
          )
        ]
      ]
      unitval
    )
  ]
  unitval
]
```

## Example: StakingCredential

The `StakingCredential` datatype is defined as follows:

```k
data StakingCredential
    = StakingHash Credential
    | StakingPtr Integer Integer Integer
```

The part of compiled code corresponding to the encoding of `StakingCredential`
is as follows:

```k
[
  [
    [
      // Selector of the StakingHash constructor
      (lam StakingHash
        // Selector of the StakingPtr constructor
        (lam StakingPtr
          // StakingCredential matcher
          (lam StakingCredential_match
            [
              // StakingCredential decoder
              (lam fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                ...
              )
              // StakingCredential decoder
              // d: input data, which has to be constructor data (unConstrData)
              (lam d
                [
                  // tup: a pair (scIdx:Int, scParams:list(data))
                  (lam tup
                    [
                      // t: scParams
                      (lam t
                        [
                          // index: scIdx
                          (lam index
                            [
                              [
                                (force (builtin ifThenElse))
                                [
                                  (builtin equalsInteger)
                                  (force index)
                                  (con integer 1)
                                ]
                                // StakingPtr constructor: three parameters
                                (lam ds
                                  [
                                    StakingPtr
                                    // First parameter
                                    [
                                      (builtin unIData)
                                      [ (force (builtin headList)) (force t) ]
                                    ]
                                    // Second parameter
                                    [
                                      (builtin unIData)
                                      [ (force (builtin headList))
                                      [ (force (builtin tailList)) (force t) ] ]
                                    ]
                                    // Third parameter
                                    [
                                      (builtin unIData)
                                      [ (force (builtin headList))
                                      [ (force (builtin tailList))
                                      [ (force (builtin tailList)) (force t) ] ] ]
                                    ]
                                  ]
                                )
                                // StakingHash constructor: one parameter
                                (lam ds
                                  [
                                    [
                                      [
                                        (force (builtin ifThenElse))
                                        [
                                          (builtin equalsInteger)
                                          (force index)
                                          (con integer 0)
                                        ]
                                        (lam ds
                                          [
                                            StakingHash
                                            // First parameter
                                            [
                                              fUnsafeFromDataCredential_cunsafeFromBuiltinData
                                              [ (force (builtin headList)) [ (force t) ] ]
                                            ]
                                          ]
                                        )
                                      ]
                                      [ THROW_ERROR_LAM reconstructCaseError ]
                                    ]
                                    unitval
                                  ]
                                )
                              ]
                              unitval
                            ]
                          )
                          // scIdx
                          (delay [ (force (force (builtin fstPair))) (force tup) ] )
                        ]
                      )
                      // scParams
                      (delay [ (force (force (builtin sndPair))) (force tup) ])
                    ]
                  )
                  // (scIdx, scParams)
                  (delay [ (builtin unConstrData) d ])
                ]
              )
            ]
          )
        )
      )
      // StakingHash selector: three parameters
      (lam arg_0
        (delay
          (lam case_StakingHash
            (lam case_StakingPtr
              [ case_StakingHash arg_0 ]
            )
          )
        )
      )
    ]
    // StakingPtr selector: three parameters
    (lam arg_0 (lam arg_1 (lam arg_2
      (delay
        (lam case_StakingHash
          (lam case_StakingPtr
            [ case_StakingPtr arg_0 arg_1 arg_2 ]
          )
        )
      )
    )))
  ]
  // StakingCredential matcher is the identity function
  (lam x x)
]
```

# Static global environment

The first part of executing compiled code is loading all of the
selectors, matchers, and decoders into the environment. This will create
an environment of considerable size, which would then be duplicated and
extended throughout the term. This is unlikely to be tractable.

However, all of the identifiers used in the compilation of datatypes are
static.  This means that we could construct an auxiliary, global environment
that holds all of the datatype-compilation-related identifiers and
then adapt lookup to go to the global environment instead of the local one
when appropriate.

## Implementation idea

Interestingly, it need not be necessary to change the definition of
configurations, but only to redefine identifier lookup. We could define the
following two functions:

```k
require "domains.md"
require "uplc-syntax.md"
require "uplc-environment.md"

module UPLC-GENVIRONMENT
  imports UPLC-ID

  // Presence in the global environment
  syntax Bool ::= #inKeysgEnv(UplcId) [function, functional]

  // Lookup in the global environment
  syntax Value ::= #gLookup(UplcId) [function]

endmodule
```

and over-ride identifier lookup in the semantics:

```k
  // Global environment takes priority
  rule <k> X:UplcId => gLookup(X) ... </k>
       <env> _ => .Map </env>
  requires #inKeysgEnv(X)

  // Then comes local environment
  rule <k> X:UplcId => #lookup(RHO, X) ... </k>
       <env> RHO => .Map </env>
  requires notBool(#inKeysgEnv(X))
   andBool X in_keys(RHO)

  // Otherwise, error
  rule <k> X:UplcId => (error) ... </k>
       <env> RHO </env>
  requires notBool(#inKeysgEnv(X))
   andBool notBool(X in_keys(RHO))
```

In a separate file, which could be automatically generated, we could then
define `#inKeysgEnv` and `#gLookup`. For example:

```k
require "uplc-genvironment.md"

module UPLC-GENVIRONMENT-INSTANCE
  imports SET
  imports UPLC-ID
  imports UPLC-MAP
  imports UPLC-SYNTAX
  imports UPLC-GENVIRONMENT

  // Global environment identifiers
  rule #inKeysgEnv(X) =>
    X in
      SetItem(id_1)
        ...
      SetItem(id_n) [simplification]

  rule #gLookup(id_1) => value_1
  ...
  rule #gLookup(id_n) => value_n

endmodule
```

## Potential problems:

1. The identifiers arising from the datatype compilation are not unique. This
   is possible if two datatype names or two datatype constructors are equal.
   Can this happen in Haskell? One solution would be to alter the compilation.

2. The identifiers of selectors, matchers, and decoders clash with other
   identifiers declared in the body of the policy. This is unlikely given 1,
   and the fact that the code comes from compilation.


# Automatic generation of datatype abstractions and related claims

To reason about real-world code, we need to be able to reason about the
associated datatypes (e.g., `StakingCredential`, `DCert`, etc.). Below is a
systematic way of formulating and reasoning about these abstractions,
which I believe can be automated. We are working with the datatype

```
data T
  = C_0 T_0_0 ... T_0_M0
  | ...
  | C_N T_N_0 ... T_N_MN
```

and as a more concrete running example use, once more, the `StakingCredential`.

## Datatype definition

We start by defining a top-level abstraction for the entire datatype:

```k
  // Definition
  syntax Value ::= << T >>(Int, List) [function, injective]
  rule << T >>(0, L:List) => << C_0 >>(L)
    ...
  rule << T >>(N, L:List) => << C_N >>(L)

  // Definedness
  syntax Bool ::= << T >>Def(Int, List) [function, functional]
  rule << T >>Def(0, L:List) => << C_0 >>Def(L)
    ...
  rule << T >>Def(N, L:List) => << C_N >>Def(L)
  rule << T >>Def(_, _) => false [owise]

  rule #Ceil(<< T >>(C, PARAMS)) => { true #Equals << T >>Def(C, PARAMS) } [simplification]
```

Basically, we declare an injective function `<< T >>(Int, List)`, where
`<< T >>(C, PARAMS)` represents the inhabitant of `T` with constructor `C`
and parameters described by `PARAMS`. We also declare a total function
`<< T >>Def(Int, List)`, which captures the definability of `<< T >>`.
These two functions simply serve as collectors for the constructor functions
and constructor definedness, and do not go into the structure of the
passed parameters. For the `StakingCredential` datatype, they are
defined as follows:

```k
  // Definition
  syntax Value ::= StakingCredential(Int, List) [function, injective]
  rule StakingCredential(0, L:List) => StakingHash(L)
  rule StakingCredential(1, L:List) => StakingPtr(L)

  // Definedness
  syntax Bool ::= StakingCredentialDef(Int, List) [function, functional]
  rule StakingCredentialDef(0, L:List) => StakingHashDef(L)
  rule StakingCredentialDef(1, L:List) => StakingPtrDef(L)
  rule StakingCredentialDef(_, _) => false [owise]

  rule #Ceil(StakingCredential(C, PARAMS)) => { true #Equals StakingCredentialDef(C, PARAMS) } [simplification]
```

## Constructors

We next define abstractions for each datatype constructor:

```k
  // Constructor I: C_I
  syntax Value ::= << C_I >>(List) [function, injective]
  rule << C_I >>(ListItem(PARAM_0) ... ListItem(PARAMS_MI)) =>
    < delay
      (lam case_<< C_0 >> ... (lam case_<< C_N >> [ case_<< C_I >> arg_0 ... arg_MI ]))
      arg_0 |-> << T_0 >>(toFunctionParams(PARAM_0))
      ...
      arg_MI |-> << T_MI >>(toFunctionParams(PARAM_MI))
    >

  syntax Bool ::= << C_I >>Def(List) [function, functional]
  rule << C_I >>Def(ListItem(PARAM_0) ... ListItem(PARAM_MI)) =>
         << T_0 >>Def(toFunctionParams(PARAM_0)) andBool
            ...
         << T_MI >>Def(toFunctionParams(PARAM_MI))

  rule << C_I >>Def(_) => false [owise]

  rule #Ceil(<< C_I >>(PARAMS)) => { true #Equals << C_I >>Def(PARAMS) } [simplification]
```
essentially instantiating the constructor selector with arguments carrying
the appropriate values. For some `T_I`, (e.g., `Integer` or `ByteString`),
these values are trivial (e.g., `< con integer PARAM >` or
`< con bytestring PARAM >`), and the corresponding definability conditions
equal `true`, but these can also involve other more complex types. The
`toFunctionParams` meta-function transforms either a single paramerter or
a list of parameters that is `PARAM_X` into a list of function parameters
at the meta-logic level. For the `StakingCredential` datatype, the
constructors are defined as follows:

```k
  // Constructor 0: StakingHash
  syntax Value ::= StakingHash(List) [function, injective]
  rule StakingHash(ListItem(ListItem(C:Int) ListItem(PARAMS:List))) =>
    < delay
      (lam case_StakingHash (lam case_StakingPtr [ case_StakingHash arg_0 ]))
      arg_0 |-> Credential(C, PARAMS)
    >

  syntax Bool ::= StakingHashDef(List) [function, functional]
  rule StakingHashDef(ListItem(ListItem(C:Int) ListItem(PARAMS:List))) => CredentialDef(C, PARAMS)
  rule StakingHashDef(_) => false [owise]

  rule #Ceil(StakingHash(PARAMS)) => { true #Equals StakingHashDef(PARAMS) } [simplification]

  // Constructor 1: StakingPtr
  syntax Value ::= StakingPtr(List) [function, injective]
  rule StakingPtr(ListItem(I1:Int) ListItem(I2:Int) ListItem(I3:Int)) =>
    < delay
      (lam case_StakingHash (lam case_StakingPtr [ case_StakingPtr arg_0 arg_1 arg_2 ]))
      arg_0 |-> < con integer I1 >
      arg_1 |-> < con integer I2 >
      arg_2 |-> < con integer I3 >
    >

  syntax Bool ::= StakingPtrDef(List) [function, functional]
  rule StakingPtrDef(ListItem(_:Int) ListItem(_:Int) ListItem(_:Int)) => true
  rule StakingPtrDef(_) => false [owise]

  rule #Ceil(StakingPtr(PARAMS)) => { true #Equals StakingPtrDef(PARAMS) } [simplification]
```

## Data

We next define what kind of data can be decoded into the values inhabiting
the considered datatype. The pattern is taken directly from the datatype
constructors:

```k
  // Textual data
  syntax TextualData ::= << T >>Data(Int, List) [function, injective]
  rule << T >>Data(C_0, ListItem(PARAM_0_0) ... ListItem(PARAM_0_M0)) =>
    Constr 0 [ << T_0_0 >>Data(toFunctionParams(PARAM_0_0)), ..., << T_0_M0 >>(toFunctionParams(PARAM_0_M0)) ]
    ...
  rule << T >>Data(C_N, ListItem(PARAM_N_0) ... ListItem(PARAM_N_MN)) =>
    Constr 0 [ << T_N_0 >>Data(toFunctionParams(PARAM_N_0)), ..., << T_N_MN >>(toFunctionParams(PARAM_N_MN)) ]

  rule #Ceil(<< T >>Data(C, PARAMS)) => { true #Equals << T >>DataDef(C, PARAMS) } [simplification]
```

and is defined as follows for `StakingCredential`:

```k
  // Textual data
  syntax TextualData ::= StakingCredentialData(Int, List) [function, injective]
  rule StakingCredentialData(0, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) =>
    Constr 0 [ CredentialData(C, PARAMS) ]
  rule StakingCredentialData(1, ListItem(I1:Int) ListItem(I2:Int) ListItem(I3:Int)) =>
    Constr 1 [ Integer I1, Integer I2, Integer I3 ]

  rule #Ceil(StakingCredentialData(C, PARAMS)) => { true #Equals StakingCredentialDef(C, PARAMS) } [simplification]
```

## Semantic summaries

More details to follow...

# Leveraging closed terms

When moving from the compute to the return phase, and when the appropriate
terms are closed, the current environment can be safely discarded and an
empty environment can be passed to the return phase instead.

More details to follow...