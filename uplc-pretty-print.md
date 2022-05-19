# UPLC Pretty-Print

```k
requires "uplc-syntax.md"

module UPLC-PRETTY-PRINT
  imports INT
  imports LIST
  imports UPLC-SYNTAX
  
  syntax Term ::= prettyPrint(Value) [function]
  syntax TermList ::= list2TermList(List) [function]
  
  rule list2TermList(.List) => .TermList

  rule list2TermList(ListItem(V:Value)) =>
       prettyPrint(V) 
  
  rule list2TermList(L:List) =>
       prettyPrint({L[0]}:>Value) list2TermList(range(L, 1, size(L)))
  requires size(L) >Int 1

  rule prettyPrint(< con T:TypeConstant C:Constant >) => (con T C) 

  rule prettyPrint(< lam I:UplcId T:Term _ >) => (lam I T) 

  rule prettyPrint(< delay T:Term _ >) => (delay T) 

  rule prettyPrint(< builtin BN:BuiltinName L:List _ >) =>
       [ (builtin BN) list2TermList(L) ] 
endmodule
```