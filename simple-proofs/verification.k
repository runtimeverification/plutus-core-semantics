requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  syntax KItem ::= simplify(KItem)
                 | simplified(KItem)

  rule #Ceil({@L:KItem}:>List) => {isList(@L) #Equals true} #And #Ceil(@L) [anywhere, simplification]

  rule (_:List ListItem(X:KItem))[-1] => X  [simplification]

  rule #Ceil(#push(@E:Map, @X:UplcId, @I:Int)) =>
       ({@X in_keys(@E) #Equals false} #Or
       {isList(@E[@X] orDefault .List) #Equals true} 
       ) #And #Ceil(@E) #And #Ceil(@X) #And #Ceil(@I) [simplification]

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

  rule isList(M:Map[ _K:KItem ] orDefault .List) => true
  requires allValuesAreList(M) [simplification]

  syntax Bool ::= allValuesAreList(Map) [function]

  rule allValuesAreList(.Map) => true
  rule allValuesAreList( _ |-> V M:Map ) => isList(V) andBool allValuesAreList(M)

endmodule
