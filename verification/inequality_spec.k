module INEQUALITY-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin equalsInt X:Int Y:Int)):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E => E ( MN . result ) |-> (con (#stringToModName("Prelude") . #stringToCon("False")) .ValList) </genv>
    <mod> MN:ModName </mod>
    <gas> G => G -Int 1 </gas>
    when X =/=Int Y
        andBool (G -Int 1 >=Int 0)

endmodule
