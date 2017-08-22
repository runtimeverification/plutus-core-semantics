module ID-SPEC
  imports PLUTUS-CORE

  // Proof that id x = x
  rule
    <k> (define result [(#stringToModName("Prelude") . id):QualN X:Int]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((#stringToModName("Prelude") . id):QualN in keys(E))

endmodule
