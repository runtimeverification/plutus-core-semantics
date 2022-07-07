requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

  syntax Term ::= "FIXED_POINT" [macro]
  rule FIXED_POINT => (lam f_0 [(lam x_0 [f_0 (lam y_0 [x_0 x_0 y_0])]) (lam x_1 [f_0 (lam y_1 [x_1 x_1 y_1])])])

  syntax Term ::= "LIST_SUM" [macro]
  rule LIST_SUM =>
    (lam f_lstSum
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
    )
endmodule
