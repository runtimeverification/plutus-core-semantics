module MAYBE-TO-EITHER-JUST-SPEC
  imports PLUTUS-CORE

  // Proof that maybeToEither x (Just y) = Right y.

  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsEither):QualN
          (#stringToModName("Prelude") . equalsInt):QualN (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . maybeToEither):QualN X:Int
            (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC Y:Int) ]
          (con (#stringToModName("Prelude") . #stringToCon("Right")):QualC Y)
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV:Map </genv>
    <gas> G => G -Int 73 </gas>
    when notBool ((#stringToModName("Prelude") . maybeToEither):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsEither):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E)) andBool
         G -Int 73 >=Int 0

endmodule
