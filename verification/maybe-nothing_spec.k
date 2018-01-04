module MAYBE-NOTHING-SPEC
  imports PLUTUS-CORE

  // Proof that maybe n j Nothing = n

  rule
    <k> [(#stringToModName("Prelude") . maybe):QualN N:Int J:QualN (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .TmList)] => N ... </k>
    <lenv> _ </lenv>
    <genv> E GENV:Map (J |-> V:Val) </genv>
    <mod> MN:ModName </mod>
    when notBool ((#stringToModName("Prelude") . maybe):QualN in keys(E))
         andBool notBool ((#stringToModName("Prelude") . #stringToCon("Nothing")) in keys(E))

endmodule
