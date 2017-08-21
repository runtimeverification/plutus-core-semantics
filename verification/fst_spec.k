module FST-SPEC
  imports PLUTUS-CORE

  // Proof that fst (pair x y) = x
  rule
    <k> (define result [(#stringToModName("Prelude") . fst):QualN (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC X:Int Y:Int)]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    requires notBool ((MN . result) in keys(E GENV))
             andBool notBool ((#stringToModName("Prelude") . fst) in keys(E))
             andBool notBool ((#stringToModName("Prelude") . #stringToCon("MkPair")) in keys(E))

endmodule
