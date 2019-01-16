module RECURSION-HELPERS
    imports PLUTUS-CORE-SYNTAX

    syntax Term ::= "#combinator"
    rule #combinator => #fix
           [macro]
endmodule

module SUM-TO-10-SPEC
    imports PLUTUS-CORE-LAZY
    imports RECURSION-HELPERS

    rule <k> [ [ #combinator
                   (lam recur dummyTyVar
                        (lam n dummyTyVar
                             [ [ [ { #case #unit }
                                   [ [(builtin equalsInteger) (con 5 ! 0)] n ]
                                 ]
                                 (lam u (all a (type) (fun a a)) (con 5 ! 0))
                               ]
                               (lam u (all a (type) (fun a a))
                                    [ [ (builtin addInteger) n ]
                                      [ recur [ [(builtin subtractInteger) n] (con 5 ! 1) ] ]
                                    ]
                               )
                             ]
                        )
                   )
               ]
               (con 5 ! 10)
             ]
          => (con 5 ! 55)
         </k>
      <env> .Map => .Map </env>
      <store> .Map => _ </store>
endmodule
