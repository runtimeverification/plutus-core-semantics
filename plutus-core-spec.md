Here, we define unit tests as reachability claims.

```k
module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE
```

Integers & Integer arithmetic
-----------------------------

```k
    rule <k> 1 ! 1    => int(1, 1)               </k>                    [specification]
    rule <k> 1 ! 128  => (error (con (integer))) </k>                    [specification]
    rule <k> 1 ! -128 => int(1, -128)            </k>                    [specification]
    rule <k> 1 ! -129 => (error (con (integer))) </k>                    [specification]

    rule <k> 2 !  32768 => (error (con (integer))) </k>                    [specification]
    rule <k> 2 ! -32768 => int(2, -32768)          </k>                    [specification]
    rule <k> 2 ! -32769 => (error (con (integer))) </k>                    [specification]
```

```k
endmodule
```
