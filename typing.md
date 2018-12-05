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
                  | "[" Type Type "]" [klabel(tyapp), seqstrict]
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

    syntax BuiltinName   ::= NullaryBuiltin | BinaryBuiltin
                           | UnaryBuiltin | TernaryBuiltin
    syntax NullaryBuiltin ::= "txhash"
    syntax UnaryBuiltin   ::= "sha2_256" | "sha3_256"
                            | "blocknum"
    syntax BinaryBuiltin  ::= "addInteger"         | "subtractInteger"
                           |  "multiplyInteger"    | "divideInteger"
                           |  "quotientInteger"    | "remainderInteger"
                           |  "modInteger"
                           |  "lessThanInteger"    | "lessThanEqualsInteger"
                           |  "greaterThanInteger" | "greaterThanEqualsInteger"
                           |  "equalsInteger"
                           |  "resizeInteger"      | "sizeOfInteger"
                           |  "intToByteString"
                           |  "concatenate"        | "takeByteString"
                           |  "dropByteString"
                           |  "resizeByteString"   | "equalsByteString"
    syntax TernaryBuiltin ::= "verifySignature"

    syntax Version ::= r"[0-9]+(\\.[0-9]+)*" [token]

    syntax Constant ::= Size "!" Int
                      | Size "!" ByteString
                      | Size

    // TODO: binders for substitution
    syntax Term ::= Var
                  | "(" "run" Term ")"
                  | "{" Term Type "}" [seqstrict]
                  | "(" "unwrap" Term ")" [strict]
                  | "[" Term Term "]" [klabel(termapp), seqstrict]
                  | "(" "error" Type ")" [strict]
                  | Value

    syntax Value ::= "(" "abs" TyVar Kind Value ")" [binder]
                   | "(" "wrap" TyVar Type Value ")" [binder, strict(3)]
                   | "(" "lam" Var Type Term ")" [binder, strict(2)]
                   | "(" "con" Constant ")"
                   | "(" "builtin" BuiltinName ")"

    syntax Program ::= "(" "program" Version Term ")"
endmodule
```

Typing
======

```k
module PLUTUS-CORE-TYPING-CONFIGURATION
    imports PLUTUS-CORE-SYNTAX-BASE

    configuration <k> $PGM:Program </k>
                  <env> .K </env>
```

Program version has no semantic meaning:

```k
    rule (program V TM) => TM
```

```k
    // `Term`s evaluate to their `Type`
    syntax Term ::= Type

    // `Type`s evaluate to `Type` with their `Kind`
    syntax KindedType ::= Type "::" Kind [klabel(kindedType)]
    syntax Type       ::= KindedType 
    syntax KResult    ::= KindedType
endmodule
```

```k
module PLUTUS-CORE-TYPING-BUILTINS
    imports PLUTUS-CORE-TYPING-CONFIGURATION
    imports SUBSTITUTION
    
    rule (con S ! _:Int) => #int((con S))
    rule (con integer) => (con integer) :: (fun (size) (type))
    rule (con size)    => (con size)    :: (fun (size) (type))
    rule (con S:Size):Type => (con S) :: (size)

    syntax Type ::= #int(Type)     [function]
                  | #size(Type)    [function]
                  | #bystr(Type)   [function]
                  | "#bool"        [function]
                  | "#IntIntInt"   [function]
                  | "#IntIntBool"  [function]
                  | "#IntIntByStr" [function]
                  | "#sha"         [function]

    syntax TyVar ::= "$s" | "$s0" | "$s1" | "$s2" | "$a"

    rule #int(T)   => tyapp((con integer), T)
    rule #size(T)  => tyapp((con size), T)
    rule #bystr(T) => tyapp((con bytestring), T)

    rule #bool
      => (all $a (type)
           (fun $a (fun $a $a)))

    rule #IntIntInt
      => (all $s (size)
           (fun #int($s) (fun #int($s) #int($s))))

    rule #IntIntBool
      => (all $s (size)
           (fun #int($s) (fun #int($s) #bool)))

    rule #IntIntByStr
      => (all $s0 (size) (all $s1 (size)
           (fun #int($s0) (fun #bystr($s1) #bystr($s1)))))

    rule #sha
      => (all $s (size)
           (fun #bystr($s) #bystr((con 32))))

    rule (builtin addInteger)       => #IntIntInt
    rule (builtin subtractInteger)  => #IntIntInt
    rule (builtin multiplyInteger)  => #IntIntInt
    rule (builtin divideInteger)    => #IntIntInt
    rule (builtin quotientInteger)  => #IntIntInt
    rule (builtin remainderInteger) => #IntIntInt

    rule (builtin lessThanInteger)          => #IntIntBool
    rule (builtin lessThanEqualsInteger)    => #IntIntBool
    rule (builtin greaterThanInteger)       => #IntIntBool
    rule (builtin greaterThanEqualsInteger) => #IntIntBool
    rule (builtin equalsInteger)            => #IntIntBool

    rule (builtin resizeInteger)
      => (all $s0 (size) (all $s1 (size)
           (fun #size($s1) (fun #int($s0) #int($s1)))))

    rule (builtin sizeOfInteger)
      => (all $s (size) (fun #int($s) #size($s)))

    rule (builtin intToByteString)
      => (all $s0 (size) (all $s1 (size)
           (fun #size($s1) (fun #int($s0) #bystr($s1)))))

    rule (builtin concatenate)
      => (all $s (size)
           (fun #bystr($s) (fun #bystr($s) #bystr($s))))

    rule (builtin takeByteString) => #IntIntByStr
    rule (builtin dropByteString) => #IntIntByStr

    rule (builtin sha2_256) => #sha
    rule (builtin sha3_256) => #sha

    rule (builtin verifySignature)
      => (all $s0 (size) (all $s1 (size) (all $s2 (size)
           (fun #bystr($s0) (fun #bystr($s1) (fun #bystr($s2) #bool))))))

    rule (builtin resizeByteString)
      => (all $s0 (size) (all $s1 (size)
           (fun #size($s1) (fun #bystr($s0) #bystr($s1)))))

    rule (builtin equalsByteString)
      => (all $s (size)
           (fun #bystr($s) (fun #bystr($s) #bool)))

    rule (builtin txhash) => #bystr((con 32))

    rule (builtin blocknum)
      => (all $s (size)
           (fun #size($s) #int($s)))

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
               | #lookup(K, K)

    syntax K ::= Var "!!" Type

    rule #lookup((ALPHA :: K) ~> REST:K, ALPHA) => ALPHA :: K
    rule #lookup((ALPHA :: K) ~> REST:K, V    ) => #lookup(REST, V)
      requires ALPHA =/=K V
    rule #lookup((X:Var !! T) ~> REST:K, X) => T
    rule #lookup((X:Var !! T) ~> REST:K, V) => #lookup(REST, V)
      requires X =/=K V

    // var
    rule <k> X => #lookup(GAMMA, X) ... </k>
         <env> GAMMA </env>
      requires isVar(X) orBool isTyVar(X)

    // abs heating
    rule <k> (abs ALPHA K TM) => TM ~> (all ALPHA K #HOLE) ... </k>
         <env> (. => (ALPHA :: K)) ~> GAMMA </env>

    // tyall heating
    rule <k> (all ALPHA K TY) => TY ~> (all ALPHA K #HOLE) ... </k>
         <env> (. => (ALPHA :: K)) ~> GAMMA </env>

    // abs cooling, tyall cooling
    rule <k> TY:Type :: (type) ~> (all ALPHA K #HOLE) => (all ALPHA K TY) :: (type) ... </k>
         <env> ((ALPHA :: K) => .) ... </env>

    // tyapp
    rule tyapp(T1 :: (fun K1 K2), T2 :: K1)  => tyapp(T1, T2) :: K2

    // inst
    rule { ((all ALPHA K T) :: (type)) (A :: K) } => T[A / ALPHA]

    // For K's builtin substitution to work properly
    syntax KVariable ::= Var

    // lam heating
    rule <k> (lam X:Var (TY:Type :: (type)) TM:Term) => TM ~> (fun (TY :: (type)) #HOLE) ... </k>
         <env> (. => (X !! TY)) ... </env>

    // lam cooling
    rule <k> TY2 :: K ~> (fun (TY1 :: (type)) #HOLE) => (fun (TY1 :: (type)) (TY2 :: K)) ... </k>
         <env> ((X !! TY1) => .) ... </env>

    // tyfun
    rule (fun (TY1 :: (type)) (TY2 :: (type))) => (fun TY1 TY2) :: (type)

    // app
    rule termapp((fun T1:Type T2:Type) :: K1, T1 :: K2) => T2

    // error
    rule (error A :: (type)) => A :: (type)

endmodule
```
