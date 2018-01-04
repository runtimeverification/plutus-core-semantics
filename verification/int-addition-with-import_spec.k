module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of add using result from external program (int-addition-lib.plcore)
  rule
    <k> (define result (builtin addInt X:Int (builtin addInt Y:Int (#stringToModName("Bar") . result):QualN))):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result):QualN |-> X +Int (Y +Int 4) </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result):QualN in keys(E GENV)) andBool notBool ((#stringToModName("Bar") . result):QualN in keys(E))

endmodule
