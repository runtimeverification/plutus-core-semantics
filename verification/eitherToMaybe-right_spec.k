module EITHER-TO-MAYBE-RIGHT-SPEC
  imports PLUTUS-CORE

  // Proof that eitherToMaybe (Right x) = Just x.

  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsMaybe):QualN (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . eitherToMaybe):QualN
            (con (#stringToModName("Prelude") . #stringToCon("Right")):QualC Y:Int) ]
          (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC Y)
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV:Map </genv>
    <gas> G => G -Int 57 </gas>
    when notBool ((#stringToModName("Prelude") . eitherToMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E)) andBool
         G -Int 57 >=Int 0

endmodule
