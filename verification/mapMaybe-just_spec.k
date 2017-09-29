module MAP-MAYBE-JUST-SPEC
  imports PLUTUS-CORE

  // Proof that mapMaybe f (Just x) = Just [f x]. Here f is assumed to be not present in the
  // prelude's global environment
  
  rule
    <k>
      [(#stringToModName("Prelude") . mapMaybe):QualN F:QualN (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X:Int)]
      => (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC [f x]) ~> _:Map ~> L ...
    </k>
    <lenv> L => x |-> X m |-> (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X) f |-> V </lenv>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <mod> MN:ModName </mod>
    <gas> _ => _ </gas>

endmodule
