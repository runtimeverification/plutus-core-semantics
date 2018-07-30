Here, we define unit tests as reachability claims.

```k
module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE
    imports PLUTUS-CORE-SYNTAX
```

Lambda Calculus
---------------

Basic application:

```k
rule <k> [ (lam x a x) (con 1 ! 1) ] => int(1, 1) </k>
     <env> .Map => .Map </env>
     <store> .Map => _ </store>
  [specification]

rule <k> [ (lam x a x) (con 1 ! 128) ] => (error (con (integer))) </k>
     <env> .Map => .Map </env>
     <store> .Map => _ </store>
  [specification]
 
rule <k> [ (lam y a x) (con 1 ! 1) ] => x ~> .Map </k>
     <env> .Map => _ </env>
     <store> .Map => _ </store>
  [specification]
```

Nested application:

```k
rule <k> [[(lam x a (lam y b x)) (con 1 ! 0)] (con 2 ! 123)] => int(1, 0) </k>
     <env> .Map => .Map </env>
     <store> .Map => _ </store>
  [specification]
```

Application uses capture-free substitution:

```k
rule <k> [ (lam x a (lam x b x)) (con 1 ! 1) ] => closure(_, x, x) </k>
     <env> .Map => .Map </env>
     <store> .Map => _ </store>
  [specification]
```

Integers & Integer arithmetic
-----------------------------

```k
rule <k> (con 1 ! 1     ) => int(1, 1)               </k>                           [specification]
rule <k> (con 1 ! 128   ) => (error (con (integer))) </k>                           [specification]
rule <k> (con 1 ! -128  ) => int(1, -128)            </k>                           [specification]
rule <k> (con 1 ! -129  ) => (error (con (integer))) </k>                           [specification]

rule <k> (con 2 !  32768) => (error (con (integer))) </k>                           [specification]
rule <k> (con 2 ! -32768) => int(2, -32768)          </k>                           [specification]
rule <k> (con 2 ! -32769) => (error (con (integer))) </k>                           [specification]
```

TODO: Could we used a specification of this form to show that this term must always
reduce completely (ideally we would be able to say "it must reduce to either a `BoundedInt` term
or an `Error` term).

```
rule <k> (con S ! V:Int) => C:KValue  </k>                                          [specification]
```

### Integer arithmetic

Addition:

```k
rule <k> [[(con addInteger) (con 1 ! 1) ] (con 1 ! 1) ] => (con 1 ! 2) </k>         [specification]
rule <k> [[(con addInteger) (con 1 ! 66)] (con 1 ! 66)] => (error (con (integer))) </k>
                                                                                    [specification]
```

Subtraction:

```k
rule <k> [[(con subtractInteger) (con 3 ! 10)] (con 3 ! 8) ] => (con 3 ! 2) </k>    [specification]
rule <k> [[(con subtractInteger) (con 3 ! 7)] (con 3 ! 10) ] => (con 3 ! -3) </k>   [specification]
rule <k> [[(con subtractInteger) (con 1 ! 66)] (con 1 ! -66) ] => (error (con (integer))) </k>
                                                                                    [specification]
```

Multiplication:

```k
rule <k> [[(con multiplyInteger) (con 3 ! 10)] (con 3 ! 8) ] => (con 3 ! 80) </k>   [specification]
rule <k> [[(con multiplyInteger) (con 1 ! 12)] (con 1 ! 11)] => (error (con (integer))) </k>
                                                                                    [specification]
```

Division:

```k
rule <k> [[(con divideInteger) (con 3 ! 10)] (con 3 ! 3) ] => (con 3 ! 3) </k>      [specification]
rule <k> [[(con divideInteger) (con 3 ! 0)] (con 3 ! 10) ] => (con 3 ! 0) </k>      [specification]
rule <k> [[(con divideInteger) (con 2 ! 66)] (con 2 ! 0) ] => (error (con (integer))) </k>
                                                                                    [specification]
```

Remainder:

```k
rule <k> [[(con remainderInteger) (con 3 ! 10)] (con 3 ! 3)] => (con 3 ! 1) </k>    [specification]
rule <k> [[(con remainderInteger) (con 3 ! 0)]  (con 3 ! 10)] => (con 3 ! 0) </k>   [specification]
rule <k> [[(con remainderInteger) (con 2 ! 66)] (con 2 ! 0) ] => (error (con (integer))) </k>
                                                                                    [specification]
```

Complex nested expressions:

```k
rule <k> [[(con addInteger) [[(con remainderInteger) (con 3 ! 10)] (con 3 ! 3)]]
                            [[(con multiplyInteger ) (con 3 ! 2 )] (con 3 ! 2)]
         ]
      => (con 3 ! 5)
    </k>
rule <k> [[(con addInteger) [[(con remainderInteger) (con 1 ! 10)] (con 1 ! 3)]]
                            [[(con multiplyInteger ) (con 1 ! 15 )] (con 1 ! 16)]
         ]
      => (error (con (integer)))
    </k>
rule <k> [[(con addInteger) [[(con remainderInteger) (con 3 ! 66)] (con 3 ! 0)]]
                            [[(con multiplyInteger ) (con 3 ! 2 )] (con 3 ! 2)]
         ]
      => (error (con (integer)))
    </k>
```

Less than:

```k
rule <k> [[(con lessThanInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>  [specification]
rule <k> [[(con lessThanInteger) (con 3 ! 3)] (con 3 ! 10)]  => #true </k>   [specification]
rule <k> [[(con lessThanInteger) (con 3 ! 10)] (con 3 ! 10)] => #false </k>  [specification]
```

Less than or equal to:

```k
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>  [specification]
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #true </k>   [specification]
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>   [specification]
```

Greater than:

```k
rule <k> [[(con greaterThanInteger) (con 3 ! 10)] (con 3 ! 3)]  => #true </k>   [specification]
rule <k> [[(con greaterThanInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>  [specification]
rule <k> [[(con greaterThanInteger) (con 3 ! 10)] (con 3 ! 10)] => #false </k>  [specification]
```

Greater than or equal to:

```k
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #true </k>   [specification]
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>  [specification]
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>   [specification]
```

Equal to

```k
rule <k> [[(con equalsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>  [specification]
rule <k> [[(con equalsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>  [specification]
rule <k> [[(con equalsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>   [specification]
```

Resize integer

```k
rule <k> [[(con resizeInteger) (con 1)] (con 2 ! 100)] => (con 1 ! 100)           </k>  [specification]
rule <k> [[(con resizeInteger) (con 1)] (con 2 ! 128)] => (error (con (integer))) </k>  [specification]
```

```k
endmodule
```
