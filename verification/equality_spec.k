module LET-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin equalsInt X:Int X)):Def => .K ... </k>
    <env> ... ENV:Map => ENV ( Foo . result ) |-> ( con (Nat . True):QualC .ValList ) ... </env>
    <mod> Foo </mod>

endmodule