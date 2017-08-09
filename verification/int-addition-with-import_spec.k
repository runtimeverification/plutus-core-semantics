module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of add using result from external program (int-addition-lib.plcore)
  rule
    <k> (define result (builtin addInt X:Int (builtin addInt Y:Int ((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Bar"):>ModName) . result):QualN))):Def => .K ... </k>
    <env> E ENV => E ENV (MN . result) |-> X +Int (Y +Int 4) </env>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E ENV)) andBool notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Bar"):>ModName) . result) in keys(E))

endmodule
