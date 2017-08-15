module CONST-SPEC
  imports PLUTUS-CORE

  // Proof that const x y = x
  rule
    <k> (define result [((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "ModConst"):>ModName) . const):QualN X:Int Y:Int]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> Y </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV)) andBool notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "ModConst"):>ModName) . result) in keys(E))
             andBool (Y ==Int 0 orBool Y =/=Int 0)

endmodule
