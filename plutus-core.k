module PLUTUS-CORE-SYNTAX
    imports INT

    syntax Name
    syntax Var           ::= Name
    syntax TyVar         ::= Name
    syntax TyBuiltinName ::= Name
    syntax BuiltinName   ::= Name

    syntax ByteString

    syntax TyBuiltinName ::= "(" "integer" ")"
    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Constant      ::= Size "!" Int        [function]
                           | Size "!" ByteString [function]
                           | Size

    syntax Term ::= "(" "run" Term ")"
                  | "[" Term TyValue "]"
                  | "[" Term Term "]"
                  | "(" "error" Type ")"
                  | Value

    syntax Value ::= "(" "fix" Var TyValue Term ")"
                   | "(" "abs" TyVar Kind Value ")"
                   | "(" "wrap" TyVar TyValue Value ")"
                   | "(" "lam" Var TyValue Term ")"
                   | "(" "con" Constant ")"

    syntax Type ::= "(" "con" TyBuiltinName ")"

    syntax TyValue

    syntax Kind

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
