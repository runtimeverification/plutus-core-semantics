module COMPOSE2-SPEC
  imports PLUTUS-CORE

  // Proof that compose2 f g x y = f (g x y). Prelude is unused.
  rule
    <k> [(MN . compose):QualN F:QualN G:QualN X:Int Y:Int] => ([f [g x y]]:Tm ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (f |-> V1) (g |-> V2) </lenv>
    <genv> ... (F |-> V1:Val) (G |-> V2:Val) ((MN . compose):QualN |-> closure (.Map, f, (lam g (lam x (lam y [f [g x y]]))))) ... </genv>
    <mod> MN:ModName </mod>

endmodule
