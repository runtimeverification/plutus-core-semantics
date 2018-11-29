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
                  | "(" "all" TyVar Kind Type ")" [binder]
                  | "(" "fix" TyVar Type ")"
                  | "[[" Type Type "]]" [seqstrict]
                  | TyValue

    syntax TyValue ::= "(" "fun" TyValue TyValue ")"
//                     | "(" "all" TyVar Kind TyValue ")"
//                     | "(" "fix" TyVar TyValue ")"
//                     | "(" "lam" TyVar Kind Type ")"
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

    // TODO: binders for substitution
    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term Type "}" [seqstrict]
                  | "(" "unwrap" Term ")" [strict]
                  | "[" Term Term "]" [seqstrict]
                  | "(" "error" Type ")" [strict]
                  | Value

    syntax Value ::= "(" "abs" TyVar Kind Value ")" [binder]
                   | "(" "wrap" TyVar Type Value ")" [binder, strict(3)]
                   | "(" "lam" Var Type Term ")" [binder, strict(2)]
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
                  <tenv> .K </tenv>
                  <kenv> .K </kenv>
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
    imports LIST

    // For K's builtin substitution to work properly
    syntax KVariable ::= TyVar

    // For strictness
    syntax KindedType ::= "#HOLE"

    syntax K ::= #lookupKind(K, TyVar)
               | #lookupType(K, Var)

    rule #lookupKind((ALPHA @ K) ~> REST:K, ALPHA) => ALPHA @ K
    rule #lookupKind((ALPHA @ K) ~> REST:K, BETA ) => #lookupKind(REST, BETA)
      requires ALPHA =/=K BETA

    syntax K ::= Var "!" Type

    rule #lookupType((X:Var ! T) ~> REST:K, X) => T
    rule #lookupType((X:Var ! T) ~> REST:K, Y) => #lookupType(REST, Y)
      requires X =/=K Y

    // var
    rule <k> X:Var => #lookupType(GAMMA, X) ... </k>
         <tenv> GAMMA </tenv>

    // tyvar
    rule <k> ALPHA:TyVar => #lookupKind(GAMMA, ALPHA) ... </k>
         <kenv> GAMMA </kenv>
       requires notBool isVar(ALPHA)

    // abs heating
    rule <k> (abs ALPHA K TM) => TM ~> (all ALPHA K #HOLE) ... </k>
         <kenv> (. => (ALPHA @ K)) ~> GAMMA </kenv>

    // tyall heating
    rule <k> (all ALPHA K TY) => TY ~> (all ALPHA K #HOLE) ... </k>
         <kenv> (. => (ALPHA @ K)) ~> GAMMA </kenv>

    // abs cooling, tyall cooling
    rule <k> TY:Type @ (type) ~> (all ALPHA K #HOLE) => (all ALPHA K TY) @ (type) ... </k>
         <kenv> ((ALPHA @ K) => .) ... </kenv>

    // tyapp
    rule [[ T1@(fun K1 K2) T2@K1 ]] => [[ T1 T2 ]] @ K2

    // inst
    rule { ((all ALPHA K T) @ (type)) (A @ K) } => T[A / ALPHA]

    syntax KResult ::= #econtext(Type)

    // wrap
    rule ( fix ALPHA A ) => #econtext( (fix ALPHA A) )

    // unwrap
    rule (unwrap ((fix ALPHA A) @ (type))) => A[(fix ALPHA A) / ALPHA]

    // For K's builtin substitution to work properly
    syntax KVariable ::= Var

    // lam heating
    rule <k> (lam X:Var (TY:Type @ (type)) TM:Term) => TM ~> (fun (TY @ (type)) #HOLE) ... </k>
         <tenv> (. => (X ! TY)) ... </tenv>

    // lam cooling
    rule <k> TY2 @ K ~> (fun (TY1 @ (type)) #HOLE) => (fun (TY1 @ (type)) (TY2 @ K)) ... </k>
         <tenv> ((X ! TY1) => .) ... </tenv>

    // tyfun
    rule (fun (TY1 @ (type)) (TY2 @ (type))) => (fun TY1 TY2) @ (type)

    // app
    rule [ (fun T1:Type T2:Type)@K1 T1@K2 ] => T2

    // error
    rule (error A @ (type)) => A @ (type)

endmodule
```
