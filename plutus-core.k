module PLUTUS-CORE-SYNTAX
    imports INT

    syntax TyBuiltinName ::= "(" "integer" ")"
    syntax Size          ::= Int // TODO: This should not allow negative integers
    syntax Constant      ::= Size "!" Int                                                [function]
                           | Size


    syntax Type      ::= "(" "con" TyBuiltinName ")"
    syntax Term       ::= "(" "error" Type ")"
                        | Value
    syntax Value      ::= "(" "con" Constant ")"

endmodule

module PLUTUS-CORE
    imports PLUTUS-CORE-SYNTAX
    configuration <k> $PGM </k>

    syntax BoundedInt ::= int(Int , Int)
    syntax Constant   ::= BoundedInt

    rule S:Int ! V:Int => int(S, V) when -2 ^Int(8 *Int S:Int -Int 1) <=Int V
                                 andBool V <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule S:Int ! V:Int => (error (con (integer)))                                           [owise]

endmodule
