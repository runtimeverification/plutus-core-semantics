module FLIP-SPEC
  imports PLUTUS-CORE

  // Proof that flip f x y = f y x. Prelude is unused.
  rule
    <k> [(MN . flip):QualN F:QualN X:Int Y:Int] => ([f y x] ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (f |-> V) </lenv>
    <genv> ... (F |-> V:Val) ((MN . flip) |-> closure(.Map, f, (lam x (lam y [f y x])))) ... </genv>
    <mod> MN:ModName </mod>
    <gas> G </gas>

endmodule
