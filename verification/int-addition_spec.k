module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of the add program.
  rule
    <k> (define result (builtin addInt X:Int Y:Int)):Def => .K ... </k>
    <lenv> _ => .Map </lenv>
    <genv> E => E (MN . result) |-> X +Int Y </genv>
      // must name E rather than using ... here,
      // so we can write the frame condition that Foo.result hasn't
      // already been defined.
    <mod> MN:ModName </mod>
    <gas> G => G -Int 1 -Int #sizeInt(X +Int Y) </gas>
    when notBool ((MN . result) in keys(E))
         andBool (G -Int 1 -Int #sizeInt(X +Int Y) >=Int 0)

endmodule
