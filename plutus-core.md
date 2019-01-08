```k
requires "krypto.k"
```

Syntax
======

We separate the parsing of `Name` tokens from the rest of the synax to reduce conflicts when
defining rules:

```k
module PLUTUS-CORE-SYNTAX
    imports PLUTUS-CORE-SYNTAX-BASE
    imports PLUTUS-CORE-ABBREVIATIONS

    syntax Name ::= r"[a-zA-Z][a-zA-Z0-9_']*" [notInRules, token, autoReject]
                  | #LowerId                  [notInRules, token, autoReject]
                  | #UpperId                  [notInRules, token, autoReject]
endmodule
```

```k
module PLUTUS-CORE-COMMON
    imports INT
    imports BUILTIN-ID-TOKENS

    syntax Name

    // TODO: This should not allow negative integers
    syntax Size ::= Int
endmodule
```

```k
module PLUTUS-CORE-SYNTAX-TYPES
    imports PLUTUS-CORE-COMMON

    syntax TyVar ::= Name

    syntax TyBuiltinName ::= Name
                           | "integer" | "bytestring" | "size"

    syntax TyConstant ::= Size
                        | TyBuiltinName

    syntax Type ::= TyVar
                  | "(" "fun" Type Type ")" [klabel(fun)]
                  | "(" "all" TyVar Kind Type ")" [klabel(all)]
                  | "(" "fix" TyVar Type ")" [klabel(fix)]
                  | "[" Type Type "]" [klabel(tyapp)]
                  | TyValue

    syntax TyValue ::= "(" "fun" TyValue TyValue ")" [klabel(fun)]
                     | "(" "all" TyVar Kind TyValue ")" [klabel(all)]
                     | "(" "fix" TyVar TyValue ")" [klabel(fix)]
                     | "(" "lam" TyVar Kind Type ")"
                     | "(" "con" TyConstant ")"
                     | NeutralTy

    syntax NeutralTy ::= TyVar
                       | "[" NeutralTy TyValue "]" [klabel(tyapp)]

    syntax Kind ::= "(" "type" ")"
                  | "(" "fun" Kind Kind ")"
                  | "(" "size" ")"
endmodule
```

```k
module PLUTUS-CORE-SYNTAX-BASE
    imports PLUTUS-CORE-SYNTAX-TYPES

    syntax Var           ::= Name
    syntax BuiltinName   ::= Name

    syntax ByteString ::= r"\\#[a-fA-F0-9][a-fA-F0-9]*" [notInRules, token, autoReject]

    syntax BuiltinName   ::= BinaryBuiltin | UnaryBuiltin
    syntax UnaryBuiltin  ::= "sha2_256" | "sha3_256"
                           | "sizeOfInteger"
    syntax BinaryBuiltin ::= "addInteger"         | "subtractInteger"
                           | "multiplyInteger"    | "divideInteger"
                           | "remainderInteger"
                           | "lessThanInteger"    | "lessThanEqualsInteger"
                           | "greaterThanInteger" | "greaterThanEqualsInteger"
                           | "equalsInteger"
                           | "resizeInteger"
                           | "intToByteString"
                           | "concatenate"        | "takeByteString"
                           | "resizeByteString"   | "equalsByteString"

    syntax Version ::= r"[0-9]+(\\.[0-9]+)*" [token]

    syntax Constant ::= Size "!" Int
                      | Size "!" ByteString
                      | Size

    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term Type "}"
                  | "(" "unwrap" Term ")"
                  // strictness application differs in strict and lazy semantics,
                  // but both are strict in the first argument
                  | "[" Term Term "]" [strict(1), klabel(termapp)]
                  | "(" "error" Type ")"
                  | "(" "abs" TyVar Kind Term ")" [klabel(abs)]
                  | Value

    syntax Value ::= "(" "abs" TyVar Kind Value ")" [klabel(abs)]
                   | "(" "wrap" TyVar Type Value ")"
                   | "(" "lam" Var Type Term ")"
                   | "(" "con" Constant ")"
                   | "(" "builtin" BuiltinName ")"

    syntax Program ::= "(" "program" Version Term ")"
endmodule
```

Semantics
=========

Configuration
-------------

```k
module PLUTUS-CORE-CONFIGURATION
    imports PLUTUS-CORE-SYNTAX-BASE
    imports PLUTUS-CORE-ABBREVIATIONS
    imports MAP
    imports STRING

    configuration <k> $PGM:Program </k>
                  <env> .Map </env>
                  <store> .Map </store>
```

Program version has no semantic meaning

```k
    rule (program V TM) => TM
```

As some Plutus builtins need to be converted to our internal representation (lambdas to closures,
bytestring tokens to the `Bytes` sort in K), our concept of `KResult`, which defines a sort of
"fully reduced" terms, differs slightly from the specification's notion of `Value`s.

```k
    syntax ResultTerm
    syntax Value   ::= ResultTerm
    syntax KResult ::= ResultTerm
endmodule
```

Lambda Calculus
---------------

We allow two different strategeies for Lambda Calculus, strict and lazy, and
implement application via closures and environments. Closure syntax and
desugaring a lambda into a closure, as well as lookup and restoring
environments, are common in both lazy and strict semantics.

```k
module PLUTUS-CORE-LAMBDA-CALCULUS-BASE
    imports PLUTUS-CORE-CONFIGURATION

    syntax Closure    ::= closure(Map, Var, Term)
    syntax ResultTerm ::= Closure

    rule <k> (lam X _ M:Term) => closure(RHO, X, M) ... </k>
         <env> RHO </env>

    rule <k> X:Var => V ... </k>
         <env> ... X |-> N ... </env>
         <store> ... N |-> V:ResultTerm ... </store>

    rule <k> _:KResult ~> (RHO:Map => .) ... </k>
         <env> _ => RHO </env>
endmodule
```

### Strict

```k
module PLUTUS-CORE-LAMBDA-CALCULUS-STRICT
    imports PLUTUS-CORE-LAMBDA-CALCULUS-BASE
```

Since we are sharing the syntax module for the strict and lazy semantics, we
need to manually implement strictness in the second argument:

```k
    context [ V:ResultTerm HOLE ]
```

In the strict semantics, applying a closure requires the second argument is
already fully evaluated.

```k
    rule <k> [closure(RHO, X, M) V:ResultTerm] => M ~> RHO' ... </k>
         <env> RHO' => RHO[X <- !N] </env>
         <store> ... .Map => (!N:Int |-> V) ... </store>
endmodule
```

### Lazy

Lazy semantics have new construct `#unevaluated`, holding a term to be evaluated
and the environment it should be evaluated in. Applying a closure no longer
requires the second argument is fully evaluated, as application is only strict
in the first argument for lazy semantics. As such, it is stored as a thunk in
the environment.

```k
module PLUTUS-CORE-LAMBDA-CALCULUS-LAZY
    imports PLUTUS-CORE-LAMBDA-CALCULUS-BASE

    // Holder for term to be evaluated in a particular map
    syntax K ::= #unevaluated(Term, Map)
               | "(" "update" Int ")"

    rule <k> [closure(RHO, X, M) TM] => M ~> RHO' ... </k>
         <env> RHO' => RHO[X <- !N] </env>
         <store> ... .Map => (!N:Int |-> #unevaluated(TM, RHO')) ... </store>

    rule <k> X:Var => #unevaluated(TM, RHO) ~> (update N) ... </k>
         <env> ... X |-> N ... </env>
         <store> ... N |-> #unevaluated(TM, RHO) ... </store>

    rule <k> #unevaluated(TM, RHO) ~> (update N)
          => TM ~> (update N) ~> RHO'
         ...
         </k>
         <env> RHO' => RHO </env>

    rule <k> V:ResultTerm ~> ((update N) => .) ... </k>
         <store> ... N |-> (_ => V) ... </store>
```

However, even though we are not strict in general, builtins need
their arguments to be evaluated fully.

```k
    context [ (builtin B:BuiltinName) HOLE ]
    context [ [ (builtin B:BinaryBuiltin) V:ResultTerm ] HOLE ]
endmodule
```

### Alternatives

Here we show how we can use stacks of environments instead of one map.

```stack
    rule <k> (lam X _ M:Term) => closure(RHO, X, M) ... </k>
         <envStack> #env(RHO) ... </envStack>

    rule <k> [closure(RHO, X, M) V:ResultTerm] => M ~> #popEnv ... </k>
         <envStack> (. => #env(RHO[X <- V])) ... </envStack>

    rule <k> X:Var => V ... </k>
         <envStack> #env(X |-> V RHO:Map) ... </envStack>

    rule <k> _:KResult ~> (#popEnv => .) ... </k>
         <envStack> (#env(RHO) => .) ... </envStack>
```

Here we show how we can use substitution instead of closures (strict).

```substitution
   rule [ (lam X _ M:Term) V:ResultTerm ] => M[V/X]
```

Builtins
--------

Common infrastructure for handling builtins.

```k
module PLUTUS-CORE-BUILTINS
    imports PLUTUS-CORE-CONFIGURATION

    syntax KItem ::= "#failure"

    rule isResultTerm((builtin B:BinaryBuiltin)) => true
    rule isResultTerm((builtin B:UnaryBuiltin )) => true
    rule isResultTerm([(builtin B:BinaryBuiltin) TM:ResultTerm]) => true
endmodule
```

Bounded Integer Arithmetic
--------------------------

```k
module PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-CONFIGURATION
    imports PLUTUS-CORE-BUILTINS

    rule isResultTerm((con S ! I:Int)) => true
    rule isResultTerm((con (I:Int):Constant)) => true

    syntax KItem ::= #mkInt(Size, Int) [function]
    rule #mkInt(S, V) => (con S ! V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule #mkInt(S, V) => #failure
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)

    // addInteger builtin
    rule [[(builtin addInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 +Int I2)

    // subtractInteger builtin
    rule [[(builtin subtractInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 -Int I2)

    // multiplyInteger builtin
    rule [[(builtin multiplyInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 *Int I2)

    // divideInteger builtin
    rule [[(builtin divideInteger) (con S ! I1:Int)] (con S ! I2:Int)] => (con S ! (I1 /Int I2))
      requires I2 =/=Int 0
    rule [[(builtin divideInteger) (con S ! I:Int)] (con S ! 0)] => #failure

    // resizeInteger builtin
    rule [[(builtin resizeInteger) (con S1:Int)] (con S2 ! I:Int)] => #mkInt(S1, I)

    // sizeOfInteger builtin
    rule [(builtin sizeOfInteger) (con S1 ! I:Int)] => ((con S1:Int)):Term
```

### Boolean expressions


```k
    syntax KItem ::= #mkBool(Bool) [function]
    rule #mkBool(true) => #true
    rule #mkBool(false) => #false

    // remainderInteger builtin
    rule [[(builtin remainderInteger) (con S ! I1:Int)] (con S ! I2:Int)] => (con S ! (I1 %Int I2))
      requires I2 =/=Int 0
    rule [[(builtin remainderInteger) (con S ! I1:Int)] (con S ! 0)] => #failure

    // lessThanInteger builtin
    rule [[(builtin lessThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #mkBool(I1 <Int I2)

    // lessThanEqualsInteger builtin
    rule [[(builtin lessThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #mkBool(I1 <=Int I2)

    // greaterThanInteger builtin
    rule [[(builtin greaterThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #mkBool(I1 >Int I2)

    // greaterThanEqualsInteger builtin
    rule [[(builtin greaterThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #mkBool(I1 >=Int I2)

    // equalsInteger builtin
    rule [[(builtin equalsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #mkBool(I1 ==Int I2)
endmodule
```

Bytestrings
-----------

```k
module PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-BUILTINS
    imports BYTES
```

TODO: Cleanup. Convert bytestring literals into their internal representation.
We:

* Remove leading `#`.
* Convert hex-encoded part into a base 16 integer (we lose information about leading zeros here).
* Convert to `Bytes`.
* Add leading zeros by padding to half the length of original hex string.

```k
    syntax Constant ::= Size "!" Bytes
    rule isResultTerm((con S:Size ! B:Bytes)) => true

    syntax String ::= ByteString2String(ByteString) [function, hook(STRING.token2string)]
    rule (con S ! BS:ByteString)
      => (con S ! padLeftBytes( Int2Bytes( String2Base( replaceFirst(ByteString2String(BS), "#", "")
                                                      , 16)
                                         , BE, Unsigned)
                              , (lengthString(replaceFirst(ByteString2String(BS), "#", "")) +Int 1) /Int 2
                              , 0))
```

`#mkByteString` checks that a bytestring is within bounds:

```k
    syntax KItem ::= #mkByteString(Int, Bytes) [function]
    rule #mkByteString(S, B) => (con S ! B)            requires lengthBytes(B) <=Int S
    rule #mkByteString(S, B) => #failure               requires lengthBytes(B)  >Int S
```


Bytestring builtins:

```k
    rule [[(builtin intToByteString) (con S1:Int):Value] (con S2 ! I:Int)]
      => #mkByteString(S1, padLeftBytes(Int2Bytes(I, BE, Unsigned), S1, 0))

    rule [[(builtin concatenate) (con S1 ! B1:Bytes)] (con S1 ! B2:Bytes)]
      => #mkByteString(S1, B1:Bytes +Bytes B2:Bytes)

    rule [[(builtin takeByteString) (con S1 ! I1)] (con S2 ! B2:Bytes)]
      => (con S2 ! substrBytes(B2, 0, I1))
      requires I1 >Int 0 andBool I1 <=Int lengthBytes(B2)
    rule [[(builtin takeByteString) (con S1 ! I1)] (con S2 ! B2:Bytes)]
      => (con S2 ! .Bytes)
      requires I1 <=Int 0
    rule [[(builtin takeByteString) (con S1 ! I1)] (con S2 ! B2:Bytes)]
      => (con S2 ! B2)
      requires I1 >Int lengthBytes(B2)

    rule [[(builtin resizeByteString) (con S1:Int)] (con S2 ! B2:Bytes)]
      => (con S1 ! B2)
      requires S1 >=Int lengthBytes(B2)

    rule [[(builtin resizeByteString) (con S1:Int)] (con S2 ! B2:Bytes)]
      => #mkByteString(S1, B2)

    rule [[(builtin equalsByteString) (con S ! B1:Bytes)] (con S ! B2:Bytes)] => #mkBool(B1 ==K B2)
```

```k
endmodule
```

Cryptographic Builtins
----------------------

```k
module PLUTUS-CORE-CRYPTOGRAPHY
    imports PLUTUS-CORE-BYTESTRINGS
    imports HASH
    rule [(builtin sha2_256) (con S ! B:Bytes)] => #mkByteString(256, Sha2_256(B))
    rule [(builtin sha3_256) (con S ! B:Bytes)] => #mkByteString(256, Sha3_256(B))
endmodule
```

Error Terms
-----------

Error terms get propogated up immediately, throwing away all remaining evaluation.

```k
module PLUTUS-CORE-ERRORS
    imports PLUTUS-CORE-CONFIGURATION

    rule <k> (error _) ~> (REST => .K) </k>
      requires REST =/=K .K
endmodule
```

Type Erasure
------------

In this specification, we ignore types and assume that the program is well typed. We must therefore
erase certain type constructs.

```k
module PLUTUS-CORE-TYPE-ERASURE
    imports PLUTUS-CORE-CONFIGURATION

    rule (abs TY KI TM)    => TM
    rule { TM TY }         => TM
    rule (unwrap TM)       => TM
    rule (wrap TVAR TY TM) => TM
endmodule
```

Abbreviations
-------------

The Plutus Core specification defines some abbreviations:

```k
module PLUTUS-CORE-ABBREVIATIONS
    imports PLUTUS-CORE-SYNTAX-BASE

    syntax TyVar ::= "$alpha" | "$a" | "$b" | "$self" | "$nat" | "$r"
    syntax Var ::= "$t" | "$f" | "$x" | "$bv" | "$s"

    syntax TyValue ::= "#unit"
    rule #unit => (all $alpha (type) (fun $alpha $alpha))

    syntax Term ::= "#one"
    rule #one => (abs $alpha (type) (lam $x $alpha $x))

    syntax Term ::= "#true"
                  | "#false"
    rule #true  => (abs $alpha (type) (lam $t $alpha (lam $f $alpha $t)))
    rule #false => (abs $alpha (type) (lam $t $alpha (lam $f $alpha $f)))

    syntax TyValue ::= "#boolean"

    syntax Term ::= "#case"
    rule #case => (abs $alpha (type)
                  (lam $bv #boolean
                  (lam $t (fun #unit $alpha)
                  (lam $f (fun #unit $alpha)
                    [
                      [ [ {$bv (fun #unit $alpha)} $t] $f ]
                      #one
                    ] ))))

    syntax Term ::= "#strict-combinator" | "#Y-combinator" | "#fix"

    rule #strict-combinator => { { (abs $a (type) (abs $b (type) (lam $f (fun (fun $a $b) (fun $a $b)) [ { (abs $a (type) (lam $s (fix $self (fun $self $a)) [ (unwrap $s) $s ])) (fun $a $b) } (wrap $self (fun $self (fun $a $b)) (lam $s (fix $self (fun $self (fun $a $b))) (lam $x $a [ [ $f [ { (abs $a (type) (lam $s (fix $self (fun $self $a)) [ (unwrap $s) $s ])) (fun $a $b) } $s ] ] $x ]))) ]))) $r } (fun (fix $nat (all $r (type) (fun $r (fun (fun $nat $r) $r)))) $r) }

    rule #Y-combinator => (lam $f $alpha [ (lam $x $alpha [$f [$x $x]])
                                           (lam $x $alpha [$f [$x $x]])
                                       ] )

endmodule
```

```k
module PLUTUS-CORE-FIX-STRICT
    imports PLUTUS-CORE-ABBREVIATIONS
    rule #fix => #strict-combinator
endmodule

module PLUTUS-CORE-FIX-LAZY
    imports PLUTUS-CORE-ABBREVIATIONS
    rule #fix => #Y-combinator
endmodule
```

Main Module
-----------

```k
module PLUTUS-CORE-BASE
    imports PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-CRYPTOGRAPHY
    imports PLUTUS-CORE-ERRORS
    imports PLUTUS-CORE-TYPE-ERASURE
```

To make reading test output easier, we clear the contents of the `<store>` cell
when all there is only a single `K` result on top of the `<k>` cell.

```k
    rule <k> T:ResultTerm </k>
         <env> .Map </env>
         <store> S => .Map </store>
      requires S =/=K .Map
endmodule

module PLUTUS-CORE-LAZY
    imports PLUTUS-CORE-BASE
    imports PLUTUS-CORE-LAMBDA-CALCULUS-LAZY
    imports PLUTUS-CORE-FIX-LAZY
endmodule

module PLUTUS-CORE-STRICT
    imports PLUTUS-CORE-BASE
    imports PLUTUS-CORE-LAMBDA-CALCULUS-STRICT
    imports PLUTUS-CORE-FIX-STRICT
endmodule
```
