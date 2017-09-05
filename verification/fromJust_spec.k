module FROM-JUST-SPEC
  imports PLUTUS-CORE

  // Proof that fromJust (Just x) = x
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . fromJust):QualN
            (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X:Int) ]
          X
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV </genv>
    <gas> G => G -Int 1 </gas>
    when notBool ((#stringToModName("Prelude") . fromJust):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E))
         andBool (G -Int 1 >=Int 0)

endmodule
