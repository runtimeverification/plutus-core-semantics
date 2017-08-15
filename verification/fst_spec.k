module FST-SPEC
  imports PLUTUS-CORE

  // Proof that fst (pair x y) = x
  rule
    <k> (define result [((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . fst):QualN (con ((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . MkPair):QualN X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(GENV)) andBool
    notBool (((#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"):>ModName) . result) in keys(E))

endmodule
