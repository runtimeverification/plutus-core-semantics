requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  rule (_:List ListItem(X:KItem))[-1] => X [simplification]

  rule isList(M:Map[ _K:KItem ] orDefault .List) => true
  requires allValuesAreList(M) [simplification]

  rule #Ceil({@L:KItem}:>List) => {isList(@L) #Equals true} #And #Ceil(@L) [anywhere, simplification]

  rule #Ceil(#push(@E:Map, @X:UplcId, @V:Value)) =>
       ( { @X in_keys(@E) #Equals false } #Or ( { @X in_keys(@E) #Equals true } #And { isValue(@E[@X]) #Equals true } ) )
       #And #Ceil(@E) #And #Ceil(@X) #And #Ceil(@V) [simplification]

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

  /*
   * These rules are needed to establish that each element of map is a list so that the prover understands that the map
   * that we use only contain lists
   */
  syntax Bool ::= allValuesAreList(Map) [function]
  rule allValuesAreList(.Map) => true
  rule allValuesAreList( _ |-> V M:Map ) => isList(V) andBool allValuesAreList(M)

endmodule
