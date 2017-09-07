module EQUALITY-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin equalsInt X:Int X)):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E => E ( MN . result ) |-> (con (#stringToModName("Prelude") . #stringToCon("True")) .ValList) </genv>
    <mod> MN:ModName </mod>
    <gas> G => G -Int 3 </gas>
    when (G -Int 3 >=Int 0)

endmodule
