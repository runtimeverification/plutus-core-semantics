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

    rule (con 1 ! 5)
      => [ (con integer) (con 1) ] @ (type)
                        
    rule (lam x [(con integer) (con 1)] x)
      => (fun [ (con integer) (con 1) ] [ (con integer) (con 1) ]) @ (type)
      
    rule (con addInteger) => (type)
endmodule
```

