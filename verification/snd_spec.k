module FST-SPEC
  imports PLUTUS-CORE

  // Proof that snd (pair x y) = y
  rule
    <k> (define result [(#stringToModName("Prelude") . snd):QualN (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result):QualN |-> Y </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV))
             andBool notBool ((#stringToModName("Prelude") . snd):QualN in keys(E))
             andBool notBool ((#stringToModName("Prelude") . #stringToCon("MkPair")):QualC in keys(E))

endmodule
