module ID-SPEC
  imports PLUTUS-CORE

  // Proof that id x = x
  rule
    <k> (define result [((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . id):QualN X:Int]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . result) in keys(E))

endmodule
