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
      => [[ (con integer) (con 1) ]] @ (type)

    // lam, tyfun
    rule (lam x [[(con integer) (con 1)]] x)
      => (fun [[ (con integer) (con 1) ]] [[ (con integer) (con 1) ]]) @ (type)

    // builtin, tyall, tyfun
    rule (con addInteger)
      => (all s (size) (fun [[ (con integer) s ]] (fun [[ (con integer) s ]] [[ (con integer) s ]]))) @ (type)

    // app
    rule [ (lam x [[(con integer) (con 1)]] x) (con 1 ! 5) ]
      => [[ (con integer) (con 1) ]] @ (type)
endmodule
```

