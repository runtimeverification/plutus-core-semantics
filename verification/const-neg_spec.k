module CONST-SPEC
  imports PLUTUS-CORE

  // Claim that const x y = y. Should fail
  rule
    <k> (define result [(#stringToModName("Prelude") . const):QualN X:Int Y:Int]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> Y </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((#stringToModName("Prelude") . const) in keys(E))
         andBool (Y ==Int 0 orBool Y =/=Int 0)

endmodule
