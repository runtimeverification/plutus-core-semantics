Syntax
======

We separate the parsing of `Name` tokens from the rest of the synax to reduce conflicts when
defining rules:

```k
module PLUTUS-CORE-SYNTAX
    imports PLUTUS-CORE-COMMON
    imports PLUTUS-CORE-ABBREVIATIONS
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

    syntax ByteString ::= r"\\#[a-fA-F0-9][a-fA-F0-9]*"               [notInRules, token, autoReject]

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
                           | "resizeByteString"   | "equalsByteString"

    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Version       ::= r"[0-9]+(\\.[0-9]+)*"                                          [token]
    syntax Constant      ::= Size "!" Int
                           | Size "!" ByteString
                           | BuiltinName
                           | Size

    syntax TyConstant    ::= Size
                           | TyBuiltinName

    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term TyValue "}"
                  | "(" "unwrap" Term ")"
                  | "[" Term Term "]"                                                   [seqstrict]
                  | Error
                  | Value
    syntax Error ::= "(" "error" Type ")"

    syntax Value ::= "(" "abs" TyVar Kind Value ")"
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

    syntax Program ::= "(" "program" Version Term ")"
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

    configuration <k> $PGM:Program </k>
                  <env> .Map </env>
                  <store> .Map </store>
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

While this implementation of Lambda Calculus leaks storage, it looks like the most reasonable option
that works for both symbolic exectution, and for both the Java and OCaml backend.

```k
module PLUTUS-CORE-LAMBDA-CALCULUS
    imports PLUTUS-CORE-CONFIGURATION

    syntax Closure    ::= closure(Map, Var, Term)
    syntax ResultTerm ::= Closure

    rule <k> (lam X _:TyValue M:Term) => closure(RHO, X, M) ... </k>
         <env> RHO </env>
    rule <k> [closure(RHO, X, M) V:ResultTerm] => M ~> RHO' ... </k>
         <env> RHO' => RHO[X <- size(STORE)] </env>
         <store> STORE => STORE (size(STORE) |-> V) </store>

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

    rule isResultTerm( (con B:BinaryBuiltin)    ) => true
    rule isResultTerm([(con B:BinaryBuiltin) TM:ResultTerm]) => true

    syntax Size ::= size(Int) [klabel(sizeConstant)] /* klabel prevents conflict with size(Set) */
    syntax ResultTerm ::= Size
    rule (con S:Int) => size(S)

    // BinaryBuiltins
    rule [[(con B:BinaryBuiltin) (error TY)] TM] => (error TY)
    rule [[(con B:BinaryBuiltin) TM] (error TY)] => (error TY)
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
    rule [[(con addInteger) int(S, V1)] int(S, V2)] => (con S ! (V1 +Int V2))

    // subtractInteger builtin
    rule [[(con subtractInteger) int(S, V1)] int(S, V2)] => (con S ! (V1 -Int V2))

    // multiplyInteger builtin
    rule [[(con multiplyInteger) int(S, V1)] int(S, V2)] => (con S ! (V1 *Int V2))

    // divideInteger builtin
    rule [[(con divideInteger) int(S, V1)] int(S, V2)] => (con S ! (V1 /Int V2))
      requires V2 =/=Int 0
    rule [[(con divideInteger) int(S, V1)] int(S, 0)] => (error (con (integer)))

    // remainderInteger builtin
    rule [[(con remainderInteger) int(S, V1)] int(S, V2)] => (con S ! (V1 %Int V2))
      requires V2 =/=Int 0
    rule [[(con remainderInteger) int(S, V1)] int(S, 0)] => (error (con (integer)))

    // lessThanInteger builtin
    rule [[(con lessThanInteger) int(S, V1)] int(S, V2)] => #true
      requires V1 <Int V2
    rule [[(con lessThanInteger) int(S, V1)] int(S, V2)] => #false
      requires V1 >=Int V2

    // lessThanEqualsInteger builtin
    rule [[(con lessThanEqualsInteger) int(S, V1)] int(S, V2)] => #true
      requires V1 <=Int V2
    rule [[(con lessThanEqualsInteger) int(S, V1)] int(S, V2)] => #false
      requires V1 >Int V2

    // greaterThanInteger builtin
    rule [[(con greaterThanInteger) int(S, V1)] int(S, V2)] => #true
      requires V1 >Int V2
    rule [[(con greaterThanInteger) int(S, V1)] int(S, V2)] => #false
      requires V1 <=Int V2

    // greaterThanEqualsInteger builtin
    rule [[(con greaterThanEqualsInteger) int(S, V1)] int(S, V2)] => #true
      requires V1 >=Int V2
    rule [[(con greaterThanEqualsInteger) int(S, V1)] int(S, V2)] => #false
      requires V1 <Int V2

    // equalsInteger builtin
    rule [[(con equalsInteger) int(S, V1)] int(S, V1)] => #true
    rule [[(con equalsInteger) int(S, V1)] int(S, V2)] => #false
      requires V1 =/=Int V2

    // resizeInteger builtin
    rule [[(con resizeInteger) size(S1)] int(S2, V)] => (con S1 ! V)
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
    rule [[(con intToByteString) size(S1:Int)] int(S2, V:Int)]
      => #bytestringSizeLengthInt(S1, S1, V)

    rule [[(con concatenate) bytestring(S1, V1)] bytestring(S1, V2)]
      => #bytestringSizeBytes(S1, V1 +Bytes V2)

    rule [[(con takeByteString) int(S1, I1)] bytestring(S2, B2)]
      => bytestring(S2, substrBytes(B2, 0, I1))
      requires I1 >Int 0 andBool I1 <=Int lengthBytes(B2)
    rule [[(con takeByteString) int(S1, I1)] bytestring(S2, B2)]
      => bytestring(S2, .Bytes)
      requires I1 <=Int 0
    rule [[(con takeByteString) int(S1, I1)] bytestring(S2, B2)]
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
    imports PLUTUS-CORE-COMMON

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
    imports PLUTUS-CORE-BOUNDED-INTEGER-ARITHMETIC
    imports PLUTUS-CORE-BYTESTRINGS
    imports PLUTUS-CORE-TYPE-ERASURE
endmodule
```
