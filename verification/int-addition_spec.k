module INT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of the add program.
  // TODO: Environment must be empty currently for rule to go through. Ideally
  // env cell should have ... on either side (or explicit E for current env)
  // but this breaks verification currently
  rule
    <k> (define result (builtin addInt X:Int Y:Int)):Def => .K ... </k>
    <env> .Map => ( Foo . result ) |-> X +Int Y </env>
    <mod> Foo </mod>

endmodule