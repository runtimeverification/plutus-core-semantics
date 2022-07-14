requires "uplc.md"
requires "domains.md"


module VERIFICATION
  imports UPLC
  imports BYTES

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

  // The body of the Z combinator
  syntax Term ::= "ZBody" [alias]
  rule ZBody => (lam x_0 [f_0 (lam y_0 [x_0 x_0 y_0])])

  // The Z combinator
  syntax Term ::= "Z" [alias]
  rule Z => (lam f_0 [ZBody ZBody])

  // Predicate for capturing the sum of an arbitrary symbolic list
  syntax Int ::= sum(ConstantList) [function]

  // Reduction
  rule sum(         .ConstantList ) => 0              [simplification]
  rule sum(I:Int, IS:ConstantList ) => I +Int sum(IS) [simplification]

  // Predicate for capturing the length of an arbitrary symbolic list
  syntax Int ::= length(ConstantList) [function, functional]

  // Reduction
  rule length(     .ConstantList ) => 0
  rule length(_, IS:ConstantList ) => 1 +Int length(IS)

  // Auxiliary predicate for all-integer lists
  syntax Bool ::= allInts(ConstantList) [function, functional]

  rule allInts(              .ConstantList) => true
  rule allInts(C:Constant, IS:ConstantList) => isInt(C) andBool allInts(IS)

  // Definedness
  rule #Ceil ( sum ( IS ) ) => { true #Equals allInts(IS) } [simplification]

  // Predicate for capturing longer lists
  syntax Constant ::= longerList(ConstantList, ConstantList) [function]

  rule longerList(.ConstantList, _) => False                                        [simplification]
  rule longerList(_, .ConstantList) => True                                         [simplification]
  rule longerList((_, XS:ConstantList), (_, YS:ConstantList)) => longerList(XS, YS) [simplification]

  //
  // Map reasoning
  //

  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification]

  rule
  { M1:Map [ K1 <- V1  ] [ K2 <- V2  ] #Equals
    M2:Map [ K1 <- V1' ] [ K2 <- V2' ] } =>
  {
    true #Equals
            V1 ==K V1' andBool V2 ==K V2'
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ]
  } [simplification]

  rule
  { M1:Map [ K1 <- V1  ] [ K2 <- V2  ] [ K3 <- V3  ] #Equals
    M2:Map [ K1 <- V1' ] [ K2 <- V2' ] [ K3 <- V3' ] } =>
  {
    true #Equals
            V1 ==K V1' andBool V2 ==K V2' andBool V3 ==K V3'
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ]
  } [simplification]

  //
  // List algorithms
  //

  // --------------------------------------------------------

  // 1. List-Free Algorithm
  syntax Term ::= "LIST_FREE" [alias]
    rule LIST_FREE =>
      [ Z (lam f_lstFree LIST_FREE_BODY) ]

  // List-Free Body
  syntax Term ::= "LIST_FREE_BODY" [alias]
  rule LIST_FREE_BODY =>
    (lam in_lst
      LIST_FREE_LOOP_BODY
    )

  // List-Free Loop Body
  syntax Term ::= "LIST_FREE_LOOP_BODY" [alias]
  rule LIST_FREE_LOOP_BODY =>
    (force
      [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_lst ]
        ( delay in_lst )
        ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
      ]
    )

  // --------------------------------------------------------

  // 2. List-Sum Algorithm
  syntax Term ::= "LIST_SUM" [alias]
    rule LIST_SUM =>
      [ Z (lam f_lstSum LIST_SUM_BODY) ]

  // List-Sum Body
  syntax Term ::= "LIST_SUM_BODY" [alias]
    rule LIST_SUM_BODY =>
      (lam in_lst
        LIST_SUM_LOOP_BODY
      )

  // List-Sum Loop Body
  syntax Term ::= "LIST_SUM_LOOP_BODY" [alias]
    rule LIST_SUM_LOOP_BODY =>
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

  // --------------------------------------------------------

  // 3. List-Length Algorithm
  syntax Term ::= "LIST_LEN" [alias]
  rule LIST_LEN =>
    [ Z (lam f_lstLen LIST_LEN_BODY) ]

  // List-Length Body
  syntax Term ::= "LIST_LEN_BODY" [alias]
  rule LIST_LEN_BODY =>
    (lam in_lst
      LIST_LEN_LOOP_BODY
    )

  // List-length Loop Body
  syntax Term ::= "LIST_LEN_LOOP_BODY" [alias]
  rule LIST_LEN_LOOP_BODY =>
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

  // --------------------------------------------------------

  // 4. Simple List-length Client
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

  // --------------------------------------------------------

  // 5. List-Longer Algorithm
  syntax Term ::= "LIST_LONGER" [alias]
    rule LIST_LONGER =>
      [ Z (lam f_lstLonger LIST_LONGER_BODY) ]

  // List-Longer body
  syntax Term ::= "LIST_LONGER_BODY" [alias]
  rule LIST_LONGER_BODY =>
    (lam in_lst1
      (lam in_lst2
        LIST_LONGER_LOOP_BODY
      )
    )

  // List-longer loop body
  syntax Term ::= "LIST_LONGER_LOOP_BODY" [alias]
  rule LIST_LONGER_LOOP_BODY =>
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

endmodule
