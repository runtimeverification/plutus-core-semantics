module CURRY-SPEC
  imports PLUTUS-CORE

  // Proof that curry f x y = f (pair x y). Prelude is unused
  rule
    <k> [(MN . curry):QualN F:QualN X:Int Y:Int] => ([f (con MN . #stringToCon("MkPair") x y)] ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (f |-> V) </lenv>
    <genv> ... (F |-> V:Val) ((MN . curry):QualN |-> closure(.Map, f, (lam x (lam y [f (con MN . #stringToCon("MkPair") x y)])))) ... </genv>
    <mod> MN:ModName </mod>

endmodule