module APPLYTO-SPEC
  imports PLUTUS-CORE

  // Proof that applyTo x f = f x. Prelude is unused.
  rule
    <k> [(MN . applyTo):QualN X:Int F:QualN] => ([f x]:Tm ~> L) ... </k>
    <lenv> L => (x |-> X) (f |-> V) </lenv>
    <genv> ... (F |-> V:Val) ((MN . applyTo):QualN |-> closure (.Map, x, (lam f [f x]))) ... </genv>
    <mod> MN:ModName </mod>

endmodule
