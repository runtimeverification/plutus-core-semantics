module SND-SPEC
  imports PLUTUS-CORE

  // Claim that snd (pair x y) = X. Should fail.
  rule
    <k> (define result [(#stringToModName("Prelude") . snd):QualN (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV))
             andBool notBool ((#stringToModName("Prelude") . snd) in keys(E))
             andBool notBool ((#stringToModName("Prelude") . #stringToCon("MkPair")) in keys(E))
             andBool (X ==Int 0 orBool X =/=Int 0)

endmodule
