module LET-SPEC
  imports PLUTUS-CORE
  
  rule
    <k> (define result (let V:Int x ((builtin addInt x x):Tm))):Def => .K ... </k>
    <env> ... ENV:Map => ENV ( Foo . result ) |-> (builtin addInt x x)[V / x] ... </env>
    <mod> Foo </mod>

endmodule