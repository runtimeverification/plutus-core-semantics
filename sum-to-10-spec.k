module RECURSION-HELPERS
    imports PLUTUS-CORE-SYNTAX

    syntax Term ::= "#combinator"
    rule #combinator => #fix
           [macro]
endmodule

module SUM-TO-10-SPEC
    imports PLUTUS-CORE
    imports RECURSION-HELPERS

    rule <k>
      [ [ [ #combinator
            (lam recur dummyTyVar
                 (lam z r
                      (lam n dummyTyVar
                           [ [ [ [ { #case #unit }
                                   [[(con equalsInteger) (con 5 ! 0)] n ]
                                 ]
                                 (lam f #unit z)
                               ]
                               [ recur [[ (con addInteger) z] n ] ]
                             ]
                             [ [(con subtractInteger) n] (con 5 ! 1) ]
                           ]
            )    )    )
          ] 
          (con 5 ! 0)
        ]
        (con 5 ! 10)
      ] => (con 5 ! 55) </k>
      <env> .Map => .Map </env>
endmodule
