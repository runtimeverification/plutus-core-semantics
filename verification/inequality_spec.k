module INEQUALITY-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin equalsInt X:Int Y:Int)):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E => E ( MN . result ):QualN |-> (con (#stringToModName("Prelude") . #stringToCon("False")) .ValList) </genv>
    <mod> MN:ModName </mod>
    when X =/=Int Y

endmodule
