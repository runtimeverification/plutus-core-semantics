module SWAP-SPEC
  imports PLUTUS-CORE

  // Proof that swap (pair x y) = pair y x
  rule
    <k> (define result [(#stringToModName("Prelude") . swap):QualN (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E GENV => E GENV (MN . result) |-> (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC Y X) </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV))
             andBool notBool ((#stringToModName("Prelude") . swap) in keys(E))
             andBool notBool ((#stringToModName("Prelude") . #stringToCon("MkPair")) in keys(E))

endmodule
