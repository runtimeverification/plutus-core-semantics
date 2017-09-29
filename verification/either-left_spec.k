module EITHER-LEFT
  imports PLUTUS-CORE

  // Proof that either f g (Left x) = [f x]. Here f and g are assumed to be not present in the
  // prelude's global environment
  
  rule
    <k>
      [(#stringToModName("Prelude") . either):QualN L:QualN R:QualN (con (#stringToModName("Prelude") . #stringToCon("Left")):QualC X:Int)]
      => [f x] ~> _:Map ~> M ...
    </k>
    <lenv> M => x |-> X e |-> (con (#stringToModName("Prelude") . #stringToCon("Left")):QualC X) f |-> V1 g |-> V2 </lenv>
    <genv> E GENV:Map (L |-> V1:Val) (R |-> V2:Val) </genv>
    <mod> MN:ModName </mod>
    <gas> _ => _ </gas>

endmodule
