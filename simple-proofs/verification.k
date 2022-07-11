requires "uplc.md"
requires "domains.md"


module VERIFICATION
  imports UPLC
  imports BYTES

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

  // The body of the Z combinator
  syntax Term ::= "ZBody" [macro]
  rule ZBody => (lam x_0 [f_0 (lam y_0 [x_0 x_0 y_0])])

  // The Z combinator
  syntax Term ::= "Z" [macro]
  rule Z => (lam f_0 [ZBody ZBody])

  // Predicate for capturing the sum of an arbitrary symbolic list
  syntax Int ::= sum(ConstantList) [function]

  // Reduction
  rule sum(         .ConstantList ) => 0              [simplification]
  rule sum(I:Int, IS:ConstantList ) => I +Int sum(IS) [simplification]

  // Predicate for capturing the length of an arbitrary symbolic list
  syntax Int ::= length(ConstantList) [function]

  // Reduction
  rule length(     .ConstantList ) => 0                 [simplification]
  rule length(_, IS:ConstantList ) => 1 +Int length(IS) [simplification]

  // Auxiliary predicate for all-integer lists
  syntax Bool ::= allInts(ConstantList) [function, functional]

  rule allInts(              .ConstantList) => true                                 
  rule allInts(C:Constant, IS:ConstantList) => C ==K { C }:>Int andBool allInts(IS) 

  // Definedness
  rule #Ceil ( sum ( IS ) ) => { true #Equals allInts(IS) } [simplification]
 
  //
  // List algorithms
  //

  // List-Free Body
  syntax Term ::= "LIST_FREE_BODY" [macro]
  rule LIST_FREE_BODY => 
    (lam in_lst (force [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_lst ]
        ( delay in_lst )
        ( delay [ f_lstFree [ (force (builtin tailList)) in_lst ] ] )
      ])
    )

  // 1. List-Free Algorithm
  syntax Term ::= "LIST_FREE" [macro]
    rule LIST_FREE =>
      (lam f_lstFree LIST_FREE_BODY)

  // List-Sum Body
  syntax Term ::= "LIST_SUM_BODY" [macro]
    rule LIST_SUM_BODY => 
      (lam in_lst (force [ (force (builtin ifThenElse))
          [ (force (builtin nullList)) in_lst ]

          ( delay (con integer 0) )

          ( delay
            [ (builtin addInteger)
              [ (force (builtin headList)) in_lst ]
              [ f_lstSum [ (force (builtin tailList)) in_lst ] ]
            ]
          )
        ])
      )

  // 2. List-Sum Algorithm
  syntax Term ::= "LIST_SUM" [macro]
    rule LIST_SUM =>
      (lam f_lstSum LIST_SUM_BODY)

  // List-Length Body
  syntax Term ::= "LIST_LEN_BODY" [macro]
    rule LIST_LEN_BODY => 
      (lam in_lst (force [ (force (builtin ifThenElse))
          [ (force (builtin nullList)) in_lst ]

          ( delay (con integer 0) )

          ( delay
            [ (builtin addInteger)
              (con integer 1)
              [ f_lstLen [ (force (builtin tailList)) in_lst ] ]
            ]
          )
        ])
      )

  // 3a. List-Length Algorithm
  syntax Term ::= "LIST_LEN" [macro]
    rule LIST_LEN =>
      (lam f_lstLen LIST_LEN_BODY)

  // 3b. Simple List-length Client 
  syntax Term ::= "TWO_LISTS_LEN_SUM" [macro]
    rule TWO_LISTS_LEN_SUM => 
    (lam in_lst1
      (lam in_lst2
        [ (builtin addInteger)
          [ Z LIST_LEN in_lst1 ] 
          [ Z LIST_LEN in_lst2 ] 
        ]
      )
    )

endmodule
