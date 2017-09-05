module MAP-MAYBE-NOTHING-SPEC
  imports PLUTUS-CORE

  // Proof that mapMaybe f Nothing = Nothing. f assumed to be not present
  // in the prelude's global environment.
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsMaybe):QualN
          [ (#stringToModName("Prelude") . mapMaybe):QualN F:QualN
            (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .ValList) ]
          (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .ValList)
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <gas> _ => _ </gas>
    when notBool ((#stringToModName("Prelude") . mapMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsMaybe):QualN in keys(E))

endmodule
