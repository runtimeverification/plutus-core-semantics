module MAYBE-JUST-SPEC
  imports PLUTUS-CORE

  // Proof that maybe n j (Just x) = [j x]. Here j is assumed to be not present
  // in the prelude's global environment.
  rule
    <k> [(#stringToModName("Prelude") . maybe):QualN N:Int J:QualN (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X:Int)] => [j x] ~> _:Map ~> L ... </k>
    <lenv> L => x |-> X m |-> (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X) n |-> N j |-> V </lenv>
    <genv> E GENV:Map (J |-> V:Val) </genv>
    <mod> MN:ModName </mod>
    when notBool ((#stringToModName("Prelude") . maybe):QualN in keys(E))
         andBool notBool ((#stringToModName("Prelude") . #stringToCon("Just")) in keys(E))

endmodule
