module FST-SPEC
  imports PLUTUS-CORE

  // Proof that snd (pair x y) = y
  rule
    <k> (define result [(stringToModName("Prelude") . fst):QualN (con (stringToModName("Prelude") . MkPair):QualC X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> Y </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV))
             andBool notBool ((stringToModName("Prelude") . fst) in keys(E))
             andBool notBool ((stringToModName("Prelude") . MkPair):QualC in keys(E))

endmodule
