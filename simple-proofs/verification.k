requires "uplc.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  rule (_:List ListItem(X:KItem))[-1] => X [simplification]

  rule #Ceil({@L:KItem}:>List) => {isList(@L) #Equals true} #And #Ceil(@L) [anywhere, simplification]

  rule #Ceil(#push(@E:Env, @X:UplcId, @I:Int)) =>
       ({@X in_keys(@E) #Equals false} #Or {isList(@E[@X] orDefault .List) #Equals true})
       #And #Ceil(@E) #And #Ceil(@X) #And #Ceil(@I) [simplification]

  rule {#uplcHash(V1:Value) #Equals #uplcHash(V2:Value)} => {V1 #Equals V2} [simplification]

  rule M:Map[K:KItem] orDefault V:KItem => V
  requires notBool(K in_keys(M)) [simplification]

endmodule
