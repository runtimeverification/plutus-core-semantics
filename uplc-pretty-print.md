# UPLC Pretty-Print

```k
requires "uplc-syntax.md"

module UPLC-PRETTY-PRINT
  imports INT
  imports LIST
  imports UPLC-SYNTAX
  
  syntax Term ::= prettyPrint(Value) [function]
  syntax Term ::= prettyPrintApp(BuiltinName, List) [function]
  syntax Term ::= prettyPrintAppAux(Term, List) [function]
  
  rule prettyPrint(< con T:TypeConstant C:Constant >) => (con T C) 

  rule prettyPrint(< lam I:UplcId T:Term _ >) => (lam I T) 

  rule prettyPrint(< delay T:Term _ >) => (delay T) 

  rule prettyPrint(< builtin BN:BuiltinName L:List _ >) =>
       prettyPrintApp(BN, L)

  rule prettyPrintApp(BN:PolyBuiltinName, ListItem(V:Value) L) =>
       prettyPrintAppAux([(force (builtin BN)) prettyPrint(V)], L)

  rule prettyPrintApp(BN:BuiltinName, ListItem(V:Value) L) =>
       prettyPrintAppAux([(builtin BN) prettyPrint(V)], L) [owise]

  rule prettyPrintAppAux(T, .List) => T
  rule prettyPrintAppAux(T, ListItem(V:Value) L:List) =>
       prettyPrintAppAux([T prettyPrint(V)], L)
endmodule
```