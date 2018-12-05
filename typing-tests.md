```k
module SPEC-IDS
    imports BUILTIN-ID-TOKENS
    syntax Name ::= #LowerId [token, autoReject]
                  | #UpperId [token, autoReject]
endmodule
```

``` {.k}
module TYPING-TESTS-SPEC
    imports PLUTUS-CORE-TYPING
    imports SPEC-IDS

    // builtin integer
    rule (con 1 ! 5)
      => kindedType( tyapp((con integer), (con 1))
                   , (type)
                   )

    // lam, tyfun
    rule (lam x tyapp((con integer), (con 1)) x)
      => kindedType( (fun tyapp((con integer), (con 1) ) tyapp( (con integer), (con 1) ))
                   , (type)
                   )

    // builtin, tyall, tyfun
    rule (builtin addInteger)
      => kindedType( (all s (size) (fun tyapp( (con integer), s ) (fun tyapp( (con integer), s ) tyapp( (con integer), s ))))
                   , (type)
                   )

    // app
    rule [ (lam x tyapp((con integer), (con 1)) x) (con 1 ! 5) ]
      => kindedType( tyapp( (con integer), (con 1) )
                   , (type)
                   )

    // abs
    rule (abs s (size) (lam x tyapp( (con integer), s ) x))
      => kindedType( (all s (size) (fun tyapp( (con integer), s ) tyapp( (con integer), s )))
                   , (type)
                   )

    // inst
    rule { (abs s (size) (lam x tyapp( (con integer), s ) x)) (con 3) }
      => kindedType( (fun tyapp( (con integer), (con 3) ) tyapp( (con integer), (con 3) ))
                   , (type)
                   )

    // error
    rule (error (fun tyapp( (con integer), (con 1) ) tyapp( (con integer), (con 1) )))
      => kindedType( (fun tyapp( (con integer), (con 1) ) tyapp( (con integer), (con 1) ))
                   , (type)
                   )

    // false
    // rule (abs a (type) (lam x a (lam y a y))) => (all a (type) (fun a (fun a a))) @ (type)

endmodule
```

