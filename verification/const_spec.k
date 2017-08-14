module CONST-SPEC
  imports PLUTUS-CORE

  // Proof that const x y = x
  rule
    <k> (define result [((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . const):QualN X:Int Y:Int]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . result) in keys(E))

endmodule
