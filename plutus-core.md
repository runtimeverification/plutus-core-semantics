```k
module PLUTUS-CORE-SYNTAX
    imports PLUTUS-CORE-COMMON
    syntax LowerName     ::= #LowerId [token, autoreject]
    syntax UpperName     ::= #UpperId [token, autoReject]
endmodule

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
    syntax BuiltinName   ::= "addInteger" | "subtractInteger"
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
    syntax KResult    ::= Error

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

module PLUTUS-CORE
    imports PLUTUS-CORE-COMMON
    configuration <k> $PGM </k>

    syntax BoundedInt ::= int(Int , Int)
    syntax Term       ::= BoundedInt
    syntax KResult    ::= BoundedInt

    rule (con S:Int ! V:Int) => int(S, V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule (con S:Int ! V:Int) => (error (con (integer)))
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)

    syntax CurriedBuiltin ::= curried(BuiltinName)
                            | curriedArg(BuiltinName, Term) [strict(2)]
    syntax Term           ::= CurriedBuiltin
    syntax KResult        ::= CurriedBuiltin

    // addInteger builtin
    rule (con addInteger) => curried(addInteger)
    rule [curried(addInteger) int(S, V)] => curriedArg(addInteger, int(S, V))
    rule [curriedArg(addInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 +Int V2))

    // subtractInteger builtin
    rule (con subtractInteger) => curried(subtractInteger)
    rule [curried(subtractInteger) int(S, V)] => curriedArg(subtractInteger, int(S, V))
    rule [curriedArg(subtractInteger, int(S, V1)) int(S, V2)] => (con S ! (V1 -Int V2))

endmodule
```
