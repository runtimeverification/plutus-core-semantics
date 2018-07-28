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

    syntax ByteString ::= r"\\#[a-fA-F0-9]([a-fA-F0-9])*"                                   [token]

    syntax TyBuiltinName ::= "(" "integer" ")"
    syntax BuiltinName   ::= BinaryIntegerBuiltin
    syntax BinaryIntegerBuiltin ::= "addInteger"       | "subtractInteger"
                                  | "multiplyInteger"  | "divideInteger"
                                  | "remainderInteger"

    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Version       ::= r"[0-9]+(.[0-9]+)*"                                            [token]
    syntax Constant      ::= Size "!" Int
                           | Size "!" ByteString
                           | BuiltinName

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
    configuration <k> $PGM </k>

    syntax KResult    ::= Error
endmodule
```

Bounded Integer Arithmetic
--------------------------

```k
module PLUTUS-CORE-ARITHMETIC
    imports PLUTUS-CORE-CONFIGURATION

    syntax BoundedInt ::= int(Int , Int)
    syntax Term       ::= BoundedInt
    syntax KResult    ::= BoundedInt

    rule (con S:Int ! V:Int) => int(S, V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule (con S:Int ! V:Int) => (error (con (integer)))
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)

    syntax CurriedBuiltinResult ::= curried(BinaryIntegerBuiltin)
                                  | curriedArg(BinaryIntegerBuiltin, Error)
                                  | curriedArg(BinaryIntegerBuiltin, BoundedInt)
    syntax KResult        ::= CurriedBuiltinResult
    syntax CurriedBuiltin ::= CurriedBuiltinResult
                            | curriedArg(BinaryIntegerBuiltin, Term)                    [strict(2)]
    syntax Term           ::= CurriedBuiltin

    // BinaryIntegerBuiltins
    rule (con B:BinaryIntegerBuiltin)                        => curried(B)
    rule [curried(B:BinaryIntegerBuiltin) TM]                => curriedArg(B, TM)
    rule [curriedArg(B:BinaryIntegerBuiltin, (error TY)) TM] => (error TY)
    rule [curriedArg(B:BinaryIntegerBuiltin, TM) (error TY)] => (error TY)

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
endmodule
```

Main Module
-----------

```k
module PLUTUS-CORE
    imports PLUTUS-CORE-ARITHMETIC
endmodule
```
