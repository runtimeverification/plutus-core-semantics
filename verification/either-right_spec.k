module EITHER-RIGHT
  imports PLUTUS-CORE

  // Proof that either f g (Right y) = [g y]. Here f and g are assumed to be not present in the
  // prelude's global environment
  
  rule
    <k>
      [(#stringToModName("Prelude") . either):QualN L:QualN R:QualN (con (#stringToModName("Prelude") . #stringToCon("Right")):QualC Y:Int)]
      => [g y] ~> _:Map ~> M ...
    </k>
    <lenv> M => y |-> Y e |-> (con (#stringToModName("Prelude") . #stringToCon("Right")):QualC Y) f |-> V1 g |-> V2 </lenv>
    <genv> E GENV:Map (L |-> V1:Val) (R |-> V2:Val) </genv>
    <mod> MN:ModName </mod>
    <gas> _ => _ </gas>

endmodule
