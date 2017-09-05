module COMPOSE-SPEC
  imports PLUTUS-CORE

  // Proof that compose f g x = f (g x). Prelude is unused.
  rule
    <k> [(MN . compose):QualN F:QualN G:QualN X:Int] => ([f [g x]] ~> L) ... </k>
    <lenv> L => (x |-> X) (f |-> V1) (g |-> V2) </lenv>
    <genv> ... (F |-> V1:Val) (G |-> V2:Val) ((MN . compose) |-> closure (.Map, f, (lam g (lam x [f [g x]])))) ... </genv>
    <mod> MN:ModName </mod>
    <gas> _ => _ </gas>

endmodule
