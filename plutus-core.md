```k
module PLUTUS-CORE-SYNTAX
    imports INT

    syntax Name          ::= r"[a-zA-Z][a-zA-Z0-9\\_']*" [token, autoReject]
    syntax Var           ::= Name
    syntax TyVar         ::= Name
    syntax TyBuiltinName ::= Name
    syntax BuiltinName   ::= Name

    syntax ByteString ::= r"\\#[a-fA-F0-9]([a-fA-F0-9])*" [token]

    syntax TyBuiltinName ::= "(" "integer" ")"
    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Version       ::= r"[0-9]+(.[0-9]+)*" [token]
    syntax Constant      ::= Size "!" Int        [function]
                           | Size "!" ByteString [function]
                           | Size

    syntax Term ::= "(" "run" Term ")"
                  | "{" Term TyValue "}"
                  | "[" Term Term "]"
                  | "(" "error" Type ")"
                  | Value

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
                  | "(" "lam" TyVar Kind Type ")"
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
    imports PLUTUS-CORE-SYNTAX
    configuration <k> $PGM </k>

    syntax BoundedInt ::= int(Int , Int)
    syntax Constant   ::= BoundedInt

    rule S:Int ! V:Int => int(S, V)
      requires -2 ^Int(8 *Int S:Int -Int 1) <=Int V andBool V  <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule S:Int ! V:Int => (error (con (integer)))
      requires -2 ^Int(8 *Int S:Int -Int 1)  >Int V orBool  V >=Int 2 ^Int(8 *Int S:Int -Int 1)

endmodule
```
