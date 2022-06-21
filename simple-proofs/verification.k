requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  /*
   * These rules are needed to establish that each element of map is a list so that the prover understands that the map
   * that we use only contain lists
   */
  syntax Bool ::= allValuesAreList(Map) [function]
  rule allValuesAreList(.Map) => true
  rule allValuesAreList( _ |-> V M:Map ) => isList(V) andBool allValuesAreList(M)

endmodule
