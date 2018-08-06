Syntax
======

We separate the parsing of `Name` tokens from the rest of the synax to reduce conflicts when
defining rules:

```k
module PLUTUS-CORE-SYNTAX
    imports PLUTUS-CORE-COMMON
    syntax LowerName     ::= #LowerId [token, autoreject]
    syntax UpperName     ::= #UpperId [token, autoReject]
endmodule
```

```k
module PLUTUS-CORE-COMMON
    imports INT
    imports BUILTIN-ID-TOKENS

    syntax LowerName
    syntax UpperName

    // TODO: make Name have same regex as in spec
    syntax Name          ::= LowerName | UpperName
    syntax Var           ::= Name
    syntax TyVar         ::= Name
    syntax TyBuiltinName ::= Name
    syntax BuiltinName   ::= Name

    syntax ByteString ::= r"`[a-fA-F0-9][a-fA-F0-9]*"               [notInRules, token, autoReject]

    syntax TyBuiltinName ::= "(" "integer" ")" | "(" "bytestring" ")"
    syntax BuiltinName   ::= BinaryBuiltin
    syntax BinaryBuiltin ::= "addInteger"         | "subtractInteger"
                           | "multiplyInteger"    | "divideInteger"
                           | "remainderInteger"
                           | "lessThanInteger"    | "lessThanEqualsInteger"
                           | "greaterThanInteger" | "greaterThanEqualsInteger"
                           | "equalsInteger"
                           | "resizeInteger"
                           | "intToByteString"
                           | "concatenate"        | "takeByteString"
                           | "resizeByteString"

    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Version       ::= r"[0-9]+(.[0-9]+)*"                                            [token]
    syntax Constant      ::= Size "!" Int
                           | Size "!" ByteString
                           | BuiltinName
                           | Size

    syntax TyConstant    ::= Size
                           | TyBuiltinName

    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term TyValue "}"
                  | "[" Term Term "]"                                                      [strict]
                  | Error
                  | Value
    syntax Error ::= "(" "error" Type ")"

    syntax Value ::= "(" "fix" Var TyValue Term ")"
                   | "(" "abs" TyVar Kind Value ")"
                   | "(" "wrap" TyVar TyValue Value ")"
                   | "(" "lam" Var TyValue Term ")"
                   | "(" "con" Constant ")"

    syntax Type ::= TyVar
                  | "(" "rec" Type ")"
                  | "(" "fun" Type Type ")"
                  | "(" "all" TyVar Kind Type ")"
                  | "(" "fix" TyVar Type ")"
                  | "[" Type Type "]"
                  | TyValue

    syntax TyValue ::= "(" "rec" TyValue ")"
                     | "(" "fun" TyValue TyValue ")"
                     | "(" "all" TyVar Kind TyValue ")"
                     | "(" "fix" TyVar TyValue ")"
                     | "(" "lam" TyVar Kind Type ")"
                     | "(" "con" TyBuiltinName ")"
                     | NeutralTy

    syntax NeutralTy ::= TyVar
                       | "[" NeutralTy TyValue "]"

    syntax Kind ::= "(" "type" ")"
                  | "(" "fun" Kind Kind ")"
                  | "(" "size" ")"

    syntax Program ::= "(" "version" Version Term ")"
endmodule
```

Semantics
=========

Configuration
-------------

```k
module PLUTUS-CORE-CONFIGURATION
    imports PLUTUS-CORE-COMMON
    imports PLUTUS-CORE-ABBREVIATIONS
    imports DOMAINS

    configuration <k> $PGM:Term </k>
                  <env> .Map </env>
                  <store> .Map </store>
```

Since our concept of `KResult` differs slightly from the specifications notion of `Value`s (e.g.
`(con (1 ! 999))` is not fully executed) we define a sort of "fully executed" terms.

```k
    syntax ResultTerm
    syntax Term    ::= ResultTerm
    syntax KResult ::= ResultTerm

    syntax ResultTerm ::= Error
endmodule
```

Lambda Calculus
---------------

While this implementation of Lambda Calculus leaks storage, it looks like the most reasonable option
that works for both symbolic exectution, and for both the Java and OCaml backend.

```k
module PLUTUS-CORE-LAMBDA-CALCULUS
    imports PLUTUS-CORE-CONFIGURATION

    syntax Closure    ::= closure(Map, Var, Term)
    syntax ResultTerm ::= Closure

    rule <k> (lam X _:TyValue M:Term) => closure(RHO, X, M) ... </k>
         <env> RHO </env>
    rule <k> [closure(RHO, X, M) V] => M ~> RHO' ... </k>
         <env> RHO' => RHO[X <- !N] </env>
         <store> ...  .Map => (!N:Int |-> V) ... </store>
    rule <k> X:Var => V ... </k>
         <env> ... X |-> N ... </env>
         <store> ... N |-> V ... </store>
    rule <k> _:KResult ~> (RHO:Map => .) ... </k>
         <env> _ => RHO </env>
endmodule
```

Builtins
--------

Common infrastructure for handling builtins.

```k
module PLUTUS-CORE-BUILTINS
    imports PLUTUS-CORE-CONFIGURATION

    syntax KResult ::= Error

    syntax CurriedBuiltinResult ::= curried(BinaryBuiltin)
                                  | curriedArg(BinaryBuiltin, ResultTerm)
    syntax ResultTerm     ::= CurriedBuiltinResult
    syntax CurriedBuiltin ::= CurriedBuiltinResult
                            | curriedArg(BinaryBuiltin, Term)                    [strict(2)]
    syntax Term           ::= CurriedBuiltin

    syntax Size ::= size(Int) [klabel(sizeConstant)] /* klabel prevents conflict with size(Set) */
    syntax ResultTerm ::= Size
    rule (con S:Int) => size(S)

    // BinaryBuiltins
    rule (con B:BinaryBuiltin)                        => curried(B)
    rule [curried(B:BinaryBuiltin) TM]                => curriedArg(B, TM)
    rule [curriedArg(B:BinaryBuiltin, (error TY)) TM] => (error TY)
    rule [curriedArg(B:BinaryBuiltin, TM) (error TY)] => (error TY)
endmodule
```

Bounded Integer Arithmetic
--------------------------

```k
module PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-CONFIGURATION

    syntax BoundedInt ::= int(Int , Int)
    syntax ResultTerm ::= BoundedInt

    rule (con S:Int ! V:Int) => int(S, V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule (con S:Int ! V:Int) => (error (con (integer)))
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)
endmodule

module PLUTUS-CORE-BOUNDED-INTEGER-ARITHMETIC
    imports PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-BUILTINS

    // addInteger builtin
    rule [curriedArg(addInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 +Int V2))

    // subtractInteger builtin
    rule [curriedArg(subtractInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 -Int V2))

    // multiplyInteger builtin
    rule [curriedArg(multiplyInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 *Int V2))

    // divideInteger builtin
    rule [curriedArg(divideInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 /Int V2))
      requires V2 =/=Int 0
    rule [curriedArg(divideInteger, int(S, V1)) int(S, 0)] => (error (con (integer)))

    // remainderInteger builtin
    rule [curriedArg(remainderInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 %Int V2))
      requires V2 =/=Int 0
    rule [curriedArg(remainderInteger, int(S, V1)) int(S, 0)] => (error (con (integer)))

    // lessThanInteger builtin
    rule [curriedArg(lessThanInteger, int(S, V1)) int(S, V2)] => #true
      requires V1 <Int V2
    rule [curriedArg(lessThanInteger, int(S, V1)) int(S, V2)] => #false
      requires V1 >=Int V2

    // lessThanEqualsInteger builtin
    rule [curriedArg(lessThanEqualsInteger, int(S, V1)) int(S, V2)] => #true
      requires V1 <=Int V2
    rule [curriedArg(lessThanEqualsInteger, int(S, V1)) int(S, V2)] => #false
      requires V1 >Int V2

    // greaterThanInteger builtin
    rule [curriedArg(greaterThanInteger, int(S, V1)) int(S, V2)] => #true
      requires V1 >Int V2
    rule [curriedArg(greaterThanInteger, int(S, V1)) int(S, V2)] => #false
      requires V1 <=Int V2

    // greaterThanEqualsInteger builtin
    rule [curriedArg(greaterThanEqualsInteger, int(S, V1)) int(S, V2)] => #true
      requires V1 >=Int V2
    rule [curriedArg(greaterThanEqualsInteger, int(S, V1)) int(S, V2)] => #false
      requires V1 <Int V2

    // equalsInteger builtin
    rule [curriedArg(equalsInteger, int(S, V1)) int(S, V1)] => #true
    rule [curriedArg(equalsInteger, int(S, V1)) int(S, V2)] => #false
      requires V1 =/=Int V2

    // resizeInteger builtin
    rule [curriedArg(resizeInteger, size(S1)) int(S2, V)] => (con S1 ! V)
endmodule
```

Bytestrings
-----------

```k
module PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-BUILTINS
    imports BYTES

    syntax ResultTerm ::= bytestring(Int, Bytes)
```

The following constructs convert various data types to byte strings, 0-padding them if they are less
than the length parameter.

```k
    syntax Term ::= #bytestringSizeString(Int, String)
                  | #bytestringSizeBytes(Int, Bytes)
                  | #bytestringSizeLengthInt(Int, Int, Int)
                  | #bytestringSizeLengthBytes(Int, Int, Bytes)

    rule #bytestringSizeString(S, STR:String)
      => #bytestringSizeLengthInt( S
                                 , (lengthString(STR) +Int 1) /Int 2
                                 , String2Base(STR, 16))
    rule #bytestringSizeLengthInt(S, L, I)
      => #bytestringSizeLengthBytes(S, L, Int2Bytes(I, BE, Unsigned))
    rule #bytestringSizeLengthBytes(S, L, B)
      => #bytestringSizeBytes(S, padLeftBytes(B, L, 0))
    rule #bytestringSizeBytes(S, B) => bytestring(S, B)           requires lengthBytes(B) <=Int S
    rule #bytestringSizeBytes(S, B) => (error (con (bytestring))) requires lengthBytes(B)  >Int S
```

Convert bytestring literals into their internal representation:

```k
    syntax String ::= ByteString2String(ByteString) [function, hook(STRING.token2string)]
    rule (con S ! BS:ByteString) => #bytestringSizeString(S, replaceFirst(ByteString2String(BS), "`", ""))
```

Bytestring builtins:

```k
    rule [curriedArg(intToByteString, size(S1:Int)) int(S2, V:Int)]
      => #bytestringSizeLengthInt(S1, S1, V)

    rule [curriedArg(concatenate, bytestring(S1, V1)) bytestring(S1, V2)]
      => #bytestringSizeBytes(S1, V1 +Bytes V2)

    rule [curriedArg(takeByteString, int(S1, I1)) bytestring(S2, B2)]
      => bytestring(S2, substrBytes(B2, 0, I1))
      requires I1 >Int 0 andBool I1 <=Int lengthBytes(B2)
    rule [curriedArg(takeByteString, int(S1, I1)) bytestring(S2, B2)]
      => bytestring(S2, .Bytes)
      requires I1 <=Int 0
    rule [curriedArg(takeByteString, int(S1, I1)) bytestring(S2, B2)]
      => bytestring(S2, B2)
      requires I1 >Int lengthBytes(B2)

    rule [curriedArg(resizeByteString, size(S1:Int)) bytestring(S2, B2)]
      => bytestring(S1, B2)
      requires S1 >=Int lengthBytes(B2)

    rule [curriedArg(resizeByteString, size(S1:Int)) bytestring(S2, B2)]
      => (error (con (bytestring)))
      requires S1 <Int lengthBytes(B2)
```

```k
endmodule
```

Type Erasure
------------

In this specification, we ignore types and assume that the program is well typed. We must therefore
erase certain type constructs.

```k
module PLUTUS-CORE-TYPE-ERASURE
    imports PLUTUS-CORE-CONFIGURATION

    rule (abs TY KI TM) => TM
endmodule
```

Abbreviations
-------------

The Plutus Core specification defines some abbreviations:

```k
module PLUTUS-CORE-ABBREVIATIONS
    imports PLUTUS-CORE-COMMON

    syntax TyVar ::= "alpha"
    syntax Var ::= "t" | "f" | "x"

    syntax TyValue ::= "#unit" [macro]
    rule #unit => (all alpha (type) (fun alpha alpha))

    syntax Term ::= "#unitval" [macro]
    rule #unitval => (abs alpha (type) (lam x alpha x))

    syntax Term ::= "#true"  [macro]
                  | "#false" [macro]
    rule #true => (abs alpha (type) (lam t (fun #unit alpha) (lam f (fun #unit alpha) [t #unitval])))
    rule #false => (abs alpha (type) (lam t (fun #unit alpha) (lam f (fun #unit alpha) [f #unitval])))
endmodule
```

Main Module
-----------

```k
module PLUTUS-CORE
    imports PLUTUS-CORE-LAMBDA-CALCULUS
    imports PLUTUS-CORE-BOUNDED-INTEGER-ARITHMETIC
    imports PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-TYPE-ERASURE
endmodule
```
