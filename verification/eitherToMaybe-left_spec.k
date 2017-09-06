module EITHER-TO-MAYBE-LEFT-SPEC
  imports PLUTUS-CORE

  // Proof that eitherToMaybe (Left x) = Nothing.

  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsMaybe):QualN (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . eitherToMaybe):QualN
            (con (#stringToModName("Prelude") . #stringToCon("Left")):QualC X:Int) ]
          (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .ValList)
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV:Map </genv>
    <gas> _ => _ </gas>
    when notBool ((#stringToModName("Prelude") . eitherToMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E))

endmodule
