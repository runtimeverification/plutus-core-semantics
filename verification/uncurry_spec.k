module UNCURRY-SPEC
  imports PLUTUS-CORE

  // Proof that uncurry f (pair x y) = f x y. Here f is assumed to be not present
  // in the prelude's global environment.
  rule
    <k> [(#stringToModName("Prelude") . uncurry):QualN F:QualN (con (#stringToModName("Prelude") . #stringToCon("MkPair")):QualC X:Int Y:Int)] => ([f x y]:Tm ~> _:Map ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (p |-> _) (f |-> V) </lenv>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <mod> MN:ModName </mod>

endmodule