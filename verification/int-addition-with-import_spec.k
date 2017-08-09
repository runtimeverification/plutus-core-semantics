module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of the add program.
  rule
    <k> (define result (builtin addInt X:Int (builtin addInt Y:Int ((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Bar"):>ModName) . result):QualN))):Def => .K ... </k>
    <env> E ENV => E ENV ( Foo . result ) |-> X +Int (Y +Int 4) </env>
    <mod> Foo </mod>
    when   notBool ((Foo . result) in keys(E ENV))
        andBool notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Bar"):>ModName) . result) in keys(E))

endmodule
