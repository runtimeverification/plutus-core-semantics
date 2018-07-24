Here, we define unit tests as reachability claims.

```k
module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE
```

Integers & Integer arithmetic
-----------------------------

```k
    rule <k> (con 1 ! 1     ) => int(1, 1)               </k>                    [specification]
    rule <k> (con 1 ! 128   ) => (error (con (integer))) </k>                    [specification]
    rule <k> (con 1 ! -128  ) => int(1, -128)            </k>                    [specification]
    rule <k> (con 1 ! -129  ) => (error (con (integer))) </k>                    [specification]

    rule <k> (con 2 !  32768) => (error (con (integer))) </k>                    [specification]
    rule <k> (con 2 ! -32768) => int(2, -32768)          </k>                    [specification]
    rule <k> (con 2 ! -32769) => (error (con (integer))) </k>                    [specification]
```

TODO: Could we used a specification of this form to show that this term must always
reduce completely (ideally we would be able to say "it must reduce to either a `BoundedInt` term
or an `Error` term).

```
    rule <k> (con S ! V:Int)  => C:KValue  </k>                                  [specification]
```

### Integer arithmetic

```k
    rule  <k> [ [(con addInteger) (con 1 ! 1)] (con 1 ! 1) ]
           => (con 1 ! 2)
         </k>                                                                    [specification]
    rule  <k> [ [(con addInteger) (con 1 ! 66)] (con 1 ! 66) ]
           => (error (con (integer)))
         </k>                                                                    [specification]
```

```k
endmodule
```
