module CURRY-SPEC
  imports PLUTUS-CORE

  // Proof that curry f x y = f (pair x y). Here f is assumed to be not present
  // in the prelude's global environment.
  rule
    <k> [(#stringToModName("Prelude") . curry):QualN F:QualN X:Int Y:Int] => ([f (con #stringToModName("Prelude") . #stringToCon("MkPair") x y)] ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (f |-> V) </lenv>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <mod> MN:ModName </mod>

endmodule