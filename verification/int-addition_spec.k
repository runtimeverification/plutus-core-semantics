module INT-ADDITION-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (builtin addInt X:Int Y:Int)):Def => .K ... </k>
    <env> ... ENV:Map => ENV ( Foo . result ) |-> X +Int Y ... </env>
    <mod> Foo </mod>

endmodule