module RECURSION-HELPERS
    imports PLUTUS-CORE-SYNTAX

    syntax Term ::= "#combinator"
    rule #combinator => { { (abs a (type) (abs b (type) (lam f (fun (fun a b) (fun a b)) [ { (abs a (type) (lam s (fix self (fun self a)) [ (unwrap s) s ])) (fun a b) } (wrap self (fun self (fun a b)) (lam s (fix self (fun self (fun a b))) (lam x a [ [ f [ { (abs a (type) (lam s (fix self (fun self a)) [ (unwrap s) s ])) (fun a b) } s ] ] x ]))) ]))) r } (fun (fix nat (all r (type) (fun r (fun (fun nat r) r)))) r) }
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
