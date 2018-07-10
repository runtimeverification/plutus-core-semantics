module PLUTUS-CORE-SYNTAX
    imports INT

    syntax BoundedInt ::= Int "!" Int                                [function]
    syntax Term       ::= "(" "error" ")"
endmodule

module PLUTUS-CORE
    imports PLUTUS-CORE-SYNTAX
    configuration <k> $PGM </k>

    syntax BoundedInt ::= int(Int , Int)

    rule S:Int ! V:Int => int(S, V) when          -2 ^Int(8 *Int S:Int -Int 1) <=Int V
                                    andBool V <Int 2 ^Int(8 *Int S:Int -Int 1)
    rule S:Int ! V:Int => (error)                                       [owise]
endmodule
