requires "uplc.md"
requires "domains.md"

module VERIFICATION
  imports UPLC
  imports BYTES

  syntax KItem ::= simplify(KItem)
                 | simplified(KItem)

  rule #Ceil({@L:KItem}:>List) => {isList(@L) #Equals true} #And #Ceil(@L) [anywhere, simplification]

endmodule
