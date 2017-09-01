module SUM-SPEC
  imports PLUTUS-CORE

  // Proof that 1 + ... + n = n(n+1)/2
  
  rule
    <k> (define result [#stringToModName("Foo") . sum N:Int 0]):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E GENV => E GENV (MN . result) |-> ((N +Int 1) *Int N /Int 2) </genv>
    <mod> MN:ModName </mod>
    when notBool ((MN . result) in keys(E GENV)) andBool notBool ((#stringToModName("Foo") . sum):QualN in keys(E))
         andBool N >=Int 0

  rule
    <k> [(#stringToModName("Foo") . sum):QualN (builtin subtractInt n 1) (builtin addInt n s):Tm] => S +Int ((N +Int 1) *Int N /Int 2) ... </k>
    <lenv> ... n |-> N:Int s |-> S:Int ... </lenv>
    <genv> E GENV:Map </genv>
    <mod> MN:ModName </mod>

endmodule
