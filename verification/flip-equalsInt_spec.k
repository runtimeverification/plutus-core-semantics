module FLIP-SPEC
  imports PLUTUS-CORE

  // Proof that flip f x y = f y x. Here F is assumed to be not present in the
  // prelude's global environment
  rule
    <k>
      [
        (#stringToModName("Prelude") . equalsInt):QualN
          [ (#stringToModName("Prelude") . flip):QualN F:QualN X:Int Y:Int ]
          [ F Y X ]
      ]
      =>
      (con (#stringToModName("Nat") . #stringToCon("True")) .ValList)
      ...
    </k>
    <lenv> .Map </lenv>
    <genv> E GENV:Map (F |-> V:Val) </genv>
    <mod> MN:ModName </mod>
    when notBool ((#stringToModName("Prelude") . flip):QualN in keys(E))

endmodule
