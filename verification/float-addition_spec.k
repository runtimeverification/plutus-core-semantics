module FLOAT-ADDITION-SPEC
  imports PLUTUS-CORE

  // Proof of the add program.
  // TODO: Environment must be empty currently for rule to go through. Ideally
  // env cell should have ... on either side (or explicit E for current env)
  // but this breaks verification currently
  rule
    <k> (define result (builtin addFloat X:PlFloat Y:PlFloat)):Def => .K ... </k>
    <env> .Map => ( Foo . result ) |-> #toPlFloat(#toFloat(X) +Float #toFloat(Y)) </env>
    <mod> Foo </mod>

endmodule