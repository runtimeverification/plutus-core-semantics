In this file, we verify higher level properties of Plutus Core.

```k
module VERIFICATION-HELPERS
    imports INT
    syntax Bool ::= isInBounds(Int, Int)
    rule isInBounds(S, V)
      => -2 ^Int(8 *Int S -Int 1) <=Int V andBool V <Int 2 ^Int(8 *Int S -Int 1)
         [macro]
endmodule
```

```k
module VERIFICATION-SPEC
    imports PLUTUS-CORE
    imports VERIFICATION-HELPERS
```

`addInteger` tests:
-------------------

```k
rule <k> [[(con addInteger) (con S ! V1) ] (con S ! V2)] => (con S ! (V1 +Int V2)) </k>
  requires isInBounds(S, V1 +Int V2)
   andBool isInBounds(S, V1)
   andBool isInBounds(S, V2)
```

```k
rule <k> [[(con addInteger) (con S ! V1) ] (con S ! V2)] => #failure ... </k>
  requires notBool(isInBounds(S, V1 +Int V2))
```

```k
endmodule
```
