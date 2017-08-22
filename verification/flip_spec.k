module FLIP-SPEC
  imports PLUTUS-CORE

  // Proof that flip f x y = f y x. Here F is assumed to be not present in the
  // prelude's global environment
  rule
    <k> [(#stringToModName("Prelude") . flip):QualN F:QualN X:Int Y:Int] => ([f y x] ~> L) ... </k>
    <lenv> L => (x |-> X) (y |-> Y) (f |-> V) </lenv>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <mod> MN:ModName </mod>

endmodule
