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
                  | "(" "fun" Type Type ")"
                  | "(" "all" TyVar Kind Type ")"
                  | "(" "fix" TyVar Type ")"
                  | "[" Type Type "]"
                  | TyValue

    syntax TyValue ::= "(" "fun" TyValue TyValue ")"
                     | "(" "all" TyVar Kind TyValue ")"
                     | "(" "fix" TyVar TyValue ")"
                     | "(" "lam" TyVar Kind Type ")"
                     | "(" "con" TyConstant ")"
                     | NeutralTy

    syntax NeutralTy ::= TyVar
                       | "[" NeutralTy TyValue "]"

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
                      | BuiltinName
                      | Size

    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term Type "}"
                  | "(" "unwrap" Term ")"
                  | "[" Term Term "]" [seqstrict]
                  | Error
                  | Value
    syntax Error ::= "(" "error" Type ")"

    syntax Value ::= "(" "abs" TyVar Kind Value ")"
                   | "(" "wrap" TyVar Type Value ")"
                   | "(" "lam" Var Type Term ")"
                   | "(" "con" Constant ")"

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
    imports DOMAINS

    configuration <k> $PGM:Program </k>
                  <env> .Map </env>
```

Program version has no semantic meaning

```k
rule (program V TM) => TM
```

Since our concept of `KResult` differs slightly from the specifications notion of `Value`s (e.g.
`(con (1 ! 999))` is not fully executed) we define a sort of "fully executed" terms.

```k
    syntax ResultTerm
    syntax Value   ::= ResultTerm
    syntax KResult ::= ResultTerm

    syntax ResultTerm ::= Error
endmodule
```

Lambda Calculus
---------------

We use implement application via closures / environment:

```k
module PLUTUS-CORE-LAMBDA-CALCULUS
    imports PLUTUS-CORE-CONFIGURATION

    syntax Closure    ::= closure(Map, Var, Term)
    syntax ResultTerm ::= Closure

    rule <k> (lam X _ M:Term) => closure(RHO, X, M) ... </k>
         <env> RHO </env>
    rule <k> [closure(RHO, X, M) V:ResultTerm] => M ~> RHO' ... </k>
         <env> RHO' => RHO[X <- V] </env>

    rule <k> X:Var => V ... </k>
         <env> ... X |-> V ... </env>

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
    syntax KItem   ::= "#failure"

    rule isResultTerm((con B:BinaryBuiltin)) => true
    rule isResultTerm((con B:UnaryBuiltin )) => true
    rule isResultTerm([(con B:BinaryBuiltin) TM:ResultTerm]) => true

    syntax Size ::= size(Int) [klabel(sizeConstant)] /* klabel prevents conflict with size(Set) */
    syntax ResultTerm ::= Size
    rule (con S:Int):Value => size(S)

    // BinaryBuiltins
    rule [[(con B:BinaryBuiltin) (error TY)] TM] => (error TY)
    rule [[(con B:BinaryBuiltin) TM] (error TY)] => (error TY)
    rule  [(con U:UnaryBuiltin)  (error TY)]     => (error TY)
endmodule
```

Bounded Integer Arithmetic
--------------------------

```k
module PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-CONFIGURATION
    imports PLUTUS-CORE-BUILTINS

    rule isResultTerm((con S ! I:Int)) => true

    syntax KItem ::= #mkInt(Size, Int) [function]
    rule #mkInt(S, V) => (con S ! V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule #mkInt(S, V) => #failure
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)

    // addInteger builtin
    rule [[(con addInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 +Int I2)

    // subtractInteger builtin
    rule [[(con subtractInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 -Int I2)

    // multiplyInteger builtin
    rule [[(con multiplyInteger) (con S ! I1)] (con S ! I2)] => #mkInt(S, I1 *Int I2)

    // divideInteger builtin
    rule [[(con divideInteger) (con S ! I1:Int)] (con S ! I2:Int)] => (con S ! (I1 /Int I2))
      requires I2 =/=Int 0
    rule [[(con divideInteger) (con S ! I:Int)] (con S ! 0)] => #failure

    // remainderInteger builtin
    rule [[(con remainderInteger) (con S ! I1:Int)] (con S ! I2:Int)] => (con S ! (I1 %Int I2))
      requires I2 =/=Int 0
    rule [[(con remainderInteger) (con S ! I1:Int)] (con S ! 0)] => #failure

    // lessThanInteger builtin
    rule [[(con lessThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #true
      requires I1 <Int I2
    rule [[(con lessThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #false
      requires I1 >=Int I2

    // lessThanEqualsInteger builtin
    rule [[(con lessThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #true
      requires I1 <=Int I2
    rule [[(con lessThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #false
      requires I1 >Int I2

    // greaterThanInteger builtin
    rule [[(con greaterThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #true
      requires I1 >Int I2
    rule [[(con greaterThanInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #false
      requires I1 <=Int I2

    // greaterThanEqualsInteger builtin
    rule [[(con greaterThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #true
      requires I1 >=Int I2
    rule [[(con greaterThanEqualsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #false
      requires I1 <Int I2

    // equalsInteger builtin
    rule [[(con equalsInteger) (con S ! I1:Int)] (con S ! I1:Int)] => #true
    rule [[(con equalsInteger) (con S ! I1:Int)] (con S ! I2:Int)] => #false
      requires I1 =/=Int I2

    // resizeInteger builtin
    rule [[(con resizeInteger) size(S1:Int)] (con S2 ! I:Int)] => #mkInt(S1, I)
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
    rule (con S ! BS:ByteString) => #bytestringSizeString(S, replaceFirst(ByteString2String(BS), "#", ""))
```

Bytestring builtins:

```k
    rule [[(con intToByteString) size(S1:Int)] (con S2 ! V:Int)]
      => #bytestringSizeLengthInt(S1, S1, V)

    rule [[(con concatenate) bytestring(S1, V1)] bytestring(S1, V2)]
      => #bytestringSizeBytes(S1, V1 +Bytes V2)

    rule [[(con takeByteString) (con S1 ! I1)] bytestring(S2, B2)]
      => bytestring(S2, substrBytes(B2, 0, I1))
      requires I1 >Int 0 andBool I1 <=Int lengthBytes(B2)
    rule [[(con takeByteString) (con S1 ! I1)] bytestring(S2, B2)]
      => bytestring(S2, .Bytes)
      requires I1 <=Int 0
    rule [[(con takeByteString) (con S1 ! I1)] bytestring(S2, B2)]
      => bytestring(S2, B2)
      requires I1 >Int lengthBytes(B2)

    rule [[(con resizeByteString) size(S1:Int)] bytestring(S2, B2)]
      => bytestring(S1, B2)
      requires S1 >=Int lengthBytes(B2)

    rule [[(con resizeByteString) size(S1:Int)] bytestring(S2, B2)]
      => (error (con (bytestring)))
      requires S1 <Int lengthBytes(B2)

    rule [[(con equalsByteString) bytestring(S, B1)] bytestring(S, B1)] => #true
    rule [[(con equalsByteString) bytestring(S, B1)] bytestring(S, B2)] => #false
      requires B1 =/=K B2
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
    rule [(con sha2_256) bytestring(S, B)] => #bytestringSizeBytes(256, Sha2_256(B))
    rule [(con sha3_256) bytestring(S, B)] => #bytestringSizeBytes(256, Sha3_256(B))
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

    syntax TyVar ::= "alpha"
    syntax Var ::= "t" | "f" | "x" | "bv"

    syntax TyValue ::= "#unit"
    rule #unit => (all alpha (type) (fun alpha alpha))

    syntax Term ::= "#unitval"
    rule #unitval => (abs alpha (type) (lam x alpha x))

    syntax Term ::= "#true"
                  | "#false"
    rule #true => (abs alpha (type) (lam t (fun #unit alpha) (lam f (fun #unit alpha) [t #unitval])))
    rule #false => (abs alpha (type) (lam t (fun #unit alpha) (lam f (fun #unit alpha) [f #unitval])))

    syntax TyValue ::= "#boolean"

    syntax Term ::= "#case"
    rule #case => (abs alpha (type) (lam bv #boolean (lam t alpha (lam f alpha
           [[ {bv alpha}
             (lam x #unit t)]
             (lam x #unit f)
           ] ))))
endmodule
```

Main Module
-----------

```k
module PLUTUS-CORE
    imports PLUTUS-CORE-LAMBDA-CALCULUS
    imports PLUTUS-CORE-BOUNDED-INTEGERS
    imports PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-CRYPTOGRAPHY
    imports PLUTUS-CORE-TYPE-ERASURE
endmodule
```
