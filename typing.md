Syntax
======

We separate the parsing of `Name` tokens from the rest of the synax to reduce conflicts when
defining rules:

```k
require "substitution.k"

module PLUTUS-CORE-TYPING-SYNTAX
    imports PLUTUS-CORE-SYNTAX-BASE

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
                  | "(" "fun" Type Type ")" [seqstrict]
                  | "(" "all" TyVar Kind Type ")"
                  | "(" "fix" TyVar Type ")"
                  | "[[" Type Type "]]" [seqstrict]
                  | TyValue

    syntax TyValue ::= "(" "fun" TyValue TyValue ")"
//                     | "(" "all" TyVar Kind TyValue ")"
                     | "(" "fix" TyVar TyValue ")"
//                     | "(" "lam" TyVar Kind Type ")"
                     | "(" "con" TyConstant ")"
                     | NeutralTy

    syntax Term ::= "(" "fun" Term Term ")" [seqstrict]
//                  | "(" "all" TyVar Kind Term ")"

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

    // TODO: binders for substitution
    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term Type "}"
                  | "(" "unwrap" Term ")"
                  | "[" Term Term "]" [seqstrict]
                  | "(" "error" Type ")"
                  | Value

    syntax Value ::= "(" "abs" TyVar Kind Value ")"
                   | "(" "wrap" TyVar Type Value ")"
                   | "(" "lam" Var Type Term ")"
                   | "(" "con" Constant ")"

    syntax Program ::= "(" "program" Version Term ")"
endmodule
```

Typing
======

```k
module PLUTUS-CORE-TYPING-CONFIGURATION
    imports PLUTUS-CORE-SYNTAX-BASE

    configuration <k> $PGM:Program </k>
```

Program version has no semantic meaning:

```k
    rule (program V TM) => TM
```

```k
    // `Term`s evaluate to their `Type`
    syntax Term ::= Type

    // `Type`s evaluate to `Type` with their `Kind`
    syntax KindedType ::= Type "@" Kind
    syntax Type       ::= KindedType 
    syntax KResult    ::= KindedType
endmodule
```

```k
module PLUTUS-CORE-TYPING-BUILTINS
    imports PLUTUS-CORE-TYPING-CONFIGURATION
    imports SUBSTITUTION
    
    rule (con S ! _:Int) => [[ (con integer) (con S) ]]
    rule (con integer) => (con integer) @ (fun (size) (type))
    rule (con S:Size):Type => (con S) @ (size)

    syntax TyVar ::= "s"
    rule (con addInteger)
      => (all s (size)
           (fun [[(con integer) s]] (fun [[(con integer) s]] [[(con integer) s]])))
endmodule
```

```k
module PLUTUS-CORE-TYPING
    imports PLUTUS-CORE-TYPING-CONFIGURATION
    imports PLUTUS-CORE-TYPING-BUILTINS

    // For K's builtin substitution to work properly
    syntax KVariable ::= TyVar

    // tyall
    // Need to have explicit heating and cooling as the `TY` in `all` needs to be reduced to a
    // kind after substituting in the kind of `ALPHA`
    syntax K ::= #allWithHole(TyVar, Kind)
    rule (all ALPHA K TY)
      => TY[ALPHA @ K / ALPHA] ~> #allWithHole(ALPHA, K)
    rule TY@(type)             ~> #allWithHole(ALPHA, K)
      => (all ALPHA K TY) @ (type)

    // tyfun
    rule (fun (TY1@(type)):Type (TY2@(type)):Type) => (fun TY1 TY2) @ (type)

    // tyapp
    rule [[ T1@(fun K1 K2) T2@K1 ]] => [[ T1 T2 ]] @ K2

    // abs
    // rule (abs ALPHA K TM) => (all ALPHA K TM[ALPHA @ K/ALPHA])

    // app
    rule [ (fun T1:Type T2:Type)@K1 T1@K2 ] => T2

    // For K's builtin substitution to work properly
    syntax KVariable ::= Var

    // lam
    rule (lam X:Var TY:Type TM:Term) => (fun TY TM[TY/X])

endmodule
```
