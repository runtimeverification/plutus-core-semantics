module FROM-MAYBE-JUST-SPEC
  imports PLUTUS-CORE

  // Proof that fromMaybe n (Just x) = x
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . fromMaybe):QualN N:Int
            (con (#stringToModName("Prelude") . #stringToCon("Just")):QualC X:Int) ]
          X
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV </genv>
    <gas> G => G -Int 1 </gas>
    when notBool ((#stringToModName("Prelude") . fromMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E))
         andBool (G -Int 1 >=Int 0)

endmodule
