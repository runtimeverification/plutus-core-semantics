module FROM-MAYBE-NOTHING-SPEC
  imports PLUTUS-CORE

  // Proof that fromMaybe n Nothing = n
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . fromMaybe):QualN N:Int
            (con (#stringToModName("Prelude") . #stringToCon("Nothing")):QualC .ValList) ]
          N
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <genv> E GENV </genv>
    <gas> G => G -Int 13 </gas>
    when notBool ((#stringToModName("Prelude") . fromMaybe):QualN in keys(E)) andBool
         notBool ((#stringToModName("Prelude") . equalsInt):QualN in keys(E))
         andBool (G -Int 13 >=Int 0)

endmodule
