module CONST-SPEC
  imports PLUTUS-CORE

  // Proof that const x y = x
  rule
    <k> (define result [(#stringToModName("Prelude") . const):QualN X:Int Y:Int]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((#stringToModName("Prelude") . const) in keys(E))

endmodule
