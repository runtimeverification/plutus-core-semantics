module FLIP-SPEC
  imports PLUTUS-CORE

  // Proof that flip f x y = f y x. Here F is assumed to be not present in the
  // prelude's global environment
  
  rule
    <k> [(#stringToModName("Prelude") . flip):QualN F:QualN X:Int Y:Int] => T ~> _:Map ~> L ... </k>
    <lenv> L => (x |-> Y) (y |-> X) </lenv>
    <genv> E GENV:Map (F |-> closure(.Map, x, (lam y T:Tm))) </genv>
    <mod> MN:ModName </mod>

  rule
    <k> [F:QualN Y:Int X:Int] => T ~> L ... </k>
    <lenv> L => (x |-> Y) (y |-> X) </lenv>
    <genv> E GENV:Map (F |-> closure(.Map, x, (lam y T:Tm))) </genv>
    <mod> MN:ModName </mod>

endmodule
