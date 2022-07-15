requires "uplc.md"
requires "domains.md"


module LIST-ALGORITHMS
  imports UPLC
  imports BYTES

  // The REC combinator
  syntax Term ::= "REC" [alias]
  rule REC => 
    (lam f_0 [ (lam s_0 [ s_0 s_0 ]) (lam s_0 (lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ])) ] )

  // Function for capturing the sum of an arbitrary symbolic list
  syntax Int ::= sum(ConstantList) [function]

  // Reduction
  rule sum(         .ConstantList ) => 0              [simplification]
  rule sum(I:Int, IS:ConstantList ) => I +Int sum(IS) [simplification]

  // Definedness
  rule #Ceil ( sum ( IS ) ) => { true #Equals allInts(IS) } [simplification]

  // Function for capturing the length of an arbitrary symbolic list
  syntax Int ::= length(ConstantList) [function, functional]

  // Reduction
  rule length(     .ConstantList ) => 0
  rule length(_, IS:ConstantList ) => 1 +Int length(IS)

  // Auxiliary predicate for all-integer lists
  syntax Bool ::= allInts(ConstantList) [function, functional]

  rule allInts(              .ConstantList) => true
  rule allInts(C:Constant, IS:ConstantList) => isInt(C) andBool allInts(IS)

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

  // 1. List-Sum Algorithm
  syntax Term ::= "LIST_SUM" [alias]
    rule LIST_SUM =>
      [ REC (lam f_lstSum LIST_SUM_BODY) ]

  // List-Sum Body
  syntax Term ::= "LIST_SUM_BODY" [alias]
    rule LIST_SUM_BODY =>
      (lam in_lst
        LIST_SUM_LOOP
      )

  // List-Sum Loop Body
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

  // --------------------------------------------------------

  // 2. List-Length Algorithm
  syntax Term ::= "LIST_LEN" [alias]
  rule LIST_LEN =>
    [ REC (lam f_lstLen LIST_LEN_BODY) ]

  // List-Length Body
  syntax Term ::= "LIST_LEN_BODY" [alias]
  rule LIST_LEN_BODY =>
    (lam in_lst
      LIST_LEN_LOOP
    )

  // List-length Loop Body
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

endmodule
