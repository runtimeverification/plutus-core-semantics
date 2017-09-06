module MAYBE-TO-EITHER-NOTHING-SPEC
  imports PLUTUS-CORE

  // Proof that maybeToEither x Nothing = Left x.

  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsEither):QualN
          (#stringToModName("Prelude") . equalsInt):QualN (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . maybeToEither):QualN X:Int
            (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .ValList) ]
          (con (#stringToModName("Prelude") . #stringToCon("Left")):QualC X)
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV:Map </genv>
    <gas> G => G -Int 1 </gas>
    when notBool ((#stringToModName("Prelude") . maybeToEither):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsEither):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E)) andBool
         G -Int 1 >=Int 0

endmodule
