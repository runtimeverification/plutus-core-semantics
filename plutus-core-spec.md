Here, we define unit tests as reachability claims.

```k
module SPEC-IDS
    imports BUILTIN-ID-TOKENS
    syntax Name ::= #LowerId [token, autoReject]
                  | #UpperId [token, autoReject]
endmodule
```

```k
module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE
    imports SPEC-IDS
```

Lambda Calculus
---------------

Basic application:

```k
rule <k> [ (lam x a x) (con 1 ! 1) ] => int(1, 1) </k>
     <env> .Map => .Map </env>

rule <k> [ (lam x a x) (con 1 ! 128) ] => (error (con (integer))) </k>
     <env> .Map => .Map </env>
 
rule <k> [ (lam y a x) (con 1 ! 1) ] => x ~> .Map </k>
     <env> .Map => _ </env>
```

Nested application:

```k
rule <k> [[(lam x a (lam y b x)) (con 1 ! 0)] (con 2 ! 123)] => int(1, 0) </k>
     <env> .Map => .Map </env>
```

Application uses capture-free substitution:

```k
rule <k> [ (lam x a (lam x b x)) (con 1 ! 1) ] => closure(_, x, x) </k>
     <env> .Map => .Map </env>
```

Integers & Integer arithmetic
-----------------------------

```k
rule <k> (con 1 ! 1     ) => int(1, 1)               </k>
rule <k> (con 1 ! 128   ) => (error (con (integer))) </k>
rule <k> (con 1 ! -128  ) => int(1, -128)            </k>
rule <k> (con 1 ! -129  ) => (error (con (integer))) </k>

rule <k> (con 2 !  32768) => (error (con (integer))) </k>
rule <k> (con 2 ! -32768) => int(2, -32768)          </k>
rule <k> (con 2 ! -32769) => (error (con (integer))) </k>
```

TODO: Could we used a specification of this form to show that this term must always
reduce completely (ideally we would be able to say "it must reduce to either a `BoundedInt` term
or an `Error` term).

```
rule <k> (con S ! V:Int) => C:KValue  </k>
```

### Integer arithmetic

Addition:

```k
rule <k> [[(con addInteger) (con 1 ! 1) ] (con 1 ! 1) ] => int(1, 2) </k>
rule <k> [[(con addInteger) (con 1 ! 66)] (con 1 ! 66)] => (error (con (integer))) </k>
```

Subtraction:

```k
rule <k> [[(con subtractInteger) (con 3 ! 10)] (con 3 ! 8) ] => int(3, 2) </k>
rule <k> [[(con subtractInteger) (con 3 ! 7)] (con 3 ! 10) ] => int(3, -3) </k>
rule <k> [[(con subtractInteger) (con 1 ! 66)] (con 1 ! -66) ] => (error (con (integer))) </k>
```

Multiplication:

```k
rule <k> [[(con multiplyInteger) (con 3 ! 10)] (con 3 ! 8) ] => int(3, 80) </k>
rule <k> [[(con multiplyInteger) (con 1 ! 12)] (con 1 ! 11)] => (error (con (integer))) </k>
```

Division:

```k
rule <k> [[(con divideInteger) (con 3 ! 10)] (con 3 ! 3) ] => int(3, 3) </k>
rule <k> [[(con divideInteger) (con 3 ! 0)] (con 3 ! 10) ] => int(3, 0) </k>
rule <k> [[(con divideInteger) (con 2 ! 66)] (con 2 ! 0) ] => (error (con (integer))) </k>
```

Remainder:

```k
rule <k> [[(con remainderInteger) (con 3 ! 10)] (con 3 ! 3)] => int(3, 1) </k>
rule <k> [[(con remainderInteger) (con 3 ! 0)]  (con 3 ! 10)] => int(3, 0) </k>
rule <k> [[(con remainderInteger) (con 2 ! 66)] (con 2 ! 0) ] => (error (con (integer))) </k>
```

Complex nested expressions:

```k
rule <k> [[(con addInteger) [[(con remainderInteger) (con 3 ! 10)] (con 3 ! 3)]]
                            [[(con multiplyInteger ) (con 3 ! 2 )] (con 3 ! 2)]
         ]
      => int(3, 5)
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
rule <k> [[(con lessThanInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>
rule <k> [[(con lessThanInteger) (con 3 ! 3)] (con 3 ! 10)]  => #true </k>
rule <k> [[(con lessThanInteger) (con 3 ! 10)] (con 3 ! 10)] => #false </k>
```

Less than or equal to:

```k
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #true </k>
rule <k> [[(con lessThanEqualsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>
```

Greater than:

```k
rule <k> [[(con greaterThanInteger) (con 3 ! 10)] (con 3 ! 3)]  => #true </k>
rule <k> [[(con greaterThanInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>
rule <k> [[(con greaterThanInteger) (con 3 ! 10)] (con 3 ! 10)] => #false </k>
```

Greater than or equal to:

```k
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #true </k>
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>
rule <k> [[(con greaterThanEqualsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>
```

Equal to

```k
rule <k> [[(con equalsInteger) (con 3 ! 10)] (con 3 ! 3)]  => #false </k>
rule <k> [[(con equalsInteger) (con 3 ! 3)] (con 3 ! 10)]  => #false </k>
rule <k> [[(con equalsInteger) (con 3 ! 10)] (con 3 ! 10)] => #true </k>
```

Resize integer

```k
rule <k> [[(con resizeInteger) (con 1)] (con 2 ! 100)] => int(1, 100)           </k>
rule <k> [[(con resizeInteger) (con 1)] (con 2 ! 128)] => (error (con (integer))) </k>
```

Booleans & Unit
---------------

`#true`:

```k
rule  <k>[[ [[(con equalsInteger) (con 3 ! 3)] (con 3 ! 3)]
              (lam x a (con 3 ! 1))] (lam x a (con 3 ! 2))]
       => int(3, 1)
     </k>
     <env> .Map => .Map </env>
```

`#false`:

```k
rule  <k> [[ [[(con equalsInteger) (con 3 ! 3)] (con 3 ! 2)]
             (lam x a (con 3 ! 1))] (lam x a (con 3 ! 2))]
       => int(3, 2)
     </k>
     <env> .Map => .Map </env>
```

Bytestrings
-----------

```k
rule <k> (con 2 ! #token("0",                "ByteString")) => bytestring(2,      0 : nilBytes) </k>
rule <k> (con 2 ! #token("00",               "ByteString")) => bytestring(2,      0 : nilBytes) </k>
rule <k> (con 2 ! #token("0000",             "ByteString")) => bytestring(2, 0  : 0 : nilBytes) </k>
rule <k> (con 2 ! #token("1000",             "ByteString")) => bytestring(2, 16 : 0 : nilBytes) </k>
rule <k> (con 2 ! #token("00000",            "ByteString")) => (error (con (bytestring))) </k>
rule <k> (con 8 ! #token("0123456789abcdef", "ByteString")) => bytestring(8, 1 : 35 : 69 : 103 : 137 : 171 : 205 : 239 : nilBytes) </k>
```

Integer to ByteString

```k
rule <k> [[(con intToByteString) (con 1 )] (con 2 ! 100)]
      => bytestring(1 , 100 : nilBytes) </k>
rule <k> [[(con intToByteString) (con 3)] (con 2 ! 100)]
      => bytestring(3, 0 : 0 : 100 : nilBytes) </k>
rule <k> [[(con intToByteString) (con 5)] (con 2 ! 100)]
      => bytestring(5, 0 : 0 : 0 : 0 : 100 : nilBytes) </k>
rule <k> [[(con intToByteString) (con 1 )] (con 2 ! 999)]
      => (error (con (bytestring))) </k>
```

TODO: The behaviour of converting negative integers to bytestrings is not specified:

```k
// rule <k> [[(con intToByteString) (con 3)] (con 2 ! -100)]
//       => bytestring(3, TODO_WHAT_GOES_HERE : 0 : 0 : nilBytes) </k>
```

Concatentate:

```k
rule <k> [ [ (con concatenate) (con 2 ! #token("01",   "ByteString")) ]
                               (con 2 ! #token("03",   "ByteString")) ]
      => bytestring(2, 01 : 03 : nilBytes) </k>
rule <k> [ [ (con concatenate) (con 2 ! #token("0102", "ByteString")) ]
                               (con 2 ! #token("0304", "ByteString")) ]
      => (error (con (bytestring))) </k>
```

`takeByteString`
: returns the prefix of `xs` of length `n`, or `xs` itself if `n > length xs`.

```k
rule <k> [[(con takeByteString) (con 1 ! 2)] (con 8 ! #token("0123456789abcdef", "ByteString"))]
      => bytestring(8, 1 : 35 : nilBytes)
     </k>
rule <k> [[(con takeByteString) (con 1 ! 31)] (con 8 ! #token("0123456789abcdef", "ByteString"))]
      => bytestring(8, 1 : 35 : 69 : 103 : 137 : 171 : 205 : 239 : nilBytes)
     </k>
rule <k> [[(con takeByteString) (con 1 ! 0)] (con 8 ! #token("0123456789abcdef", "ByteString"))]
      => bytestring(8, nilBytes)
     </k>
// This is the observed Haskell behaviour for negative lengths.
rule <k> [[(con takeByteString) (con 1 ! -1)] (con 8 ! #token("0123456789abcdef", "ByteString"))]
      => bytestring(8, nilBytes)
     </k>
```

Resize ByteString

```k
rule <k> [[(con resizeByteString) (con 3)] (con 5 ! #token("abcdef", "ByteString"))]
      => bytestring (3, 171 : 205 : 239 : nilBytes ) </k>

rule <k> [[(con resizeByteString) (con 5)] (con 3 ! #token("abcdef", "ByteString"))]
      => bytestring (5, 171 : 205 : 239 : nilBytes ) </k>

rule <k> [[(con resizeByteString) (con 2)] (con 5 ! #token("abcdef", "ByteString"))]
      => (error (con (bytestring))) </k>
```

Equals (ByteStrings)

```k
rule <k> [[(con equalsByteString) (con 3 ! #token("abcd", "ByteString"))]
                                  (con 3 ! #token("abcde", "ByteString"))]
      => #false </k>

rule <k> [[(con equalsByteString) (con 3 ! #token("abcde", "ByteString"))]
                                  (con 3 ! #token("abcd", "ByteString"))]
      => #false </k>

rule <k> [[(con equalsByteString) (con 2 ! #token("0001", "ByteString"))]
                                  (con 2 ! #token("01", "ByteString"))]
      => #false </k>

rule <k> [[(con equalsByteString) (con 3 ! #token("abcd", "ByteString"))]
                                  (con 3 ! #token("abcd", "ByteString"))]
      => #true </k>

rule <k> [[(con equalsByteString) (con 2 ! #token("abcd", "ByteString"))]
                                  (con 1 ! #token("abcd", "ByteString"))]
      => (error (con (bytestring))) </k>

```

Cryptographic constructs
------------------------

```todo
rule <k> [(con sha2_256) (con 8 ! `0123456789abcdef)]
      // TODO: Verify that this is the correct SHA2
      => bytestring ( 256 , 85   : 197 : 63  : 93  : 73  : 2
                          : 151  : 144 : 12  : 239 : 168 : 37
                          : 208  : 200 : 232 : 233 : 83  : 46
                          : 232  : 161 : 24  : 171 : 231 : 216
                          : 87   : 7   : 98  : 205 : 56  : 190
                          : 152  : 24  : nilBytes)
    </k>                                                                            [specification]
```

```k
endmodule
```
