module APPLYTO-SPEC
  imports PLUTUS-CORE

  // Proof that applyTo x f = f x. Here f is assumed to be not present in the
  // prelude's global environment
  
  rule
    <k> [(#stringToModName("Prelude") . applyTo):QualN X:Int F:QualN] => T ~> _:Map ~> L ... </k>
    <lenv> L => (x |-> X) </lenv>
    <genv> E GENV:Map (F |-> closure(.Map, x, T:Tm)) </genv>
    <mod> MN:ModName </mod>

  rule
    <k> [F:QualN X:Int] => T ~> L ... </k>
    <lenv> L => (x |-> X) </lenv>
    <genv> E GENV:Map (F |-> closure(.Map, x, T:Tm)) </genv>
    <mod> MN:ModName </mod>

endmodule
