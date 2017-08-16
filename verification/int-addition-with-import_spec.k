module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of add using result from external program (int-addition-lib.plcore)
  rule
    <k> (define result (builtin addInt X:Int (builtin addInt Y:Int (stringToModName("Bar") . result):QualN))):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X +Int (Y +Int 4) </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((stringToModName("Bar") . result) in keys(E))

endmodule
