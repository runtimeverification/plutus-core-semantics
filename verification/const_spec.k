module CONST-SPEC
  imports PLUTUS-CORE

  // Proof that const x y = x
  rule
    <k> (define result [(#stringToModName("Prelude") . const):QualN X:Int Y:Int]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result) |-> X </genv>
    <mod> MN:ModName </mod>
    <gas> G => G -Int 1 -Int 2 *Int #sizeInt(X) -Int #sizeInt(Y) </gas>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((#stringToModName("Prelude") . const):QualN in keys(E))
         andBool (G -Int 1 -Int 2 *Int #sizeInt(X) -Int #sizeInt(Y) >=Int 0)

endmodule
