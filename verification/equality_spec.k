module LET-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin equalsInt X:Int X)):Def => .K ... </k>
    <lenv> _ </lenv>
    <genv> E => E ( MN . result ) |-> ( con (Nat . True):QualC .ValList ):Val </genv>
    <mod> MN </mod>
    when notBool ((MN . result) in keys(E))

endmodule