module ID-SPEC
  imports PLUTUS-CORE

  // Proof that id x = x
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . id):QualN X:Int ]
          X
      ]
      =>
      (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList)
      ...
    </k>
    <lenv> .Map </lenv>
    <genv> E GENV => E GENV </genv>
    <mod> MN:ModName </mod>
    when notBool ((#stringToModName("Prelude") . id):QualN in keys(E))

endmodule
