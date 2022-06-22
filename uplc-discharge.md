# UPLC Pretty-Print

```k
requires "uplc-syntax.md"

module UPLC-DISCHARGE
  imports INT
  imports LIST
  imports UPLC-SYNTAX

  syntax Term ::= dischargeTerm(Term, Map, Map) [function]
  syntax Term ::= dischargeTermApp(TermList, Term, Map, Map) [function]
  syntax Term ::= discharge(Value, Map) [function]
  syntax Term ::= dischargeApp(BuiltinName, List, Map) [function]
  syntax Term ::= dischargeAppAux(Term, List, Map) [function]
  
  rule discharge(< con T:TypeConstant C:Constant >, _) => (con T C)  

  rule discharge(< lam I:UplcId T:Term RHO:Map >, Heap:Map) => (lam I dischargeTerm(T, RHO, Heap)) 

  rule discharge(< delay T:Term RHO:Map >, Heap:Map) => (delay dischargeTerm(T, RHO, Heap)) 

  rule discharge(< builtin BN:BuiltinName L:List _ >, Heap:Map) =>
       dischargeApp(BN, L, Heap)

  rule dischargeApp(BN:PolyBuiltinName, ListItem(V:Value) L:List, Heap:Map) =>
       dischargeAppAux([(force (builtin BN)) discharge(V, Heap)], L, Heap)

  rule dischargeApp(BN:BuiltinName, ListItem(V:Value) L:List, Heap:Map) =>
       dischargeAppAux([(builtin BN) discharge(V, Heap)], L, Heap) [owise]

  rule dischargeAppAux(T:Term, .List, _:Map) => T
  rule dischargeAppAux(T:Term, ListItem(V:Value) L:List, Heap:Map) =>
       dischargeAppAux([T discharge(V, Heap)], L, Heap:Map)

  rule dischargeTerm(X:UplcId, _:Map X |-> ListItem(I:Int), Heap:Map I |-> V:Value) =>
       discharge(V, Heap I |-> V:Value)
  
  rule dischargeTerm(X:UplcId, RHO:Map, _:Map) => X
  requires notBool(X in_keys(RHO))

  rule dischargeTerm((con T:TypeConstant C:Constant), _, _) => (con T C)

  rule dischargeTerm((builtin BN:BuiltinName), _, _) => (builtin BN)

  rule dischargeTerm((lam X:UplcId T:Term), RHO:Map, Heap:Map) => (lam X dischargeTerm(T, RHO, Heap))

  rule dischargeTerm([ T1:Term (T2:Term TL:TermList) ], RHO:Map, Heap:Map) =>
       dischargeTermApp(TL, [dischargeTerm(T1, RHO, Heap) dischargeTerm(T2, RHO, Heap) ], RHO, Heap)

  rule dischargeTermApp(T1:Term TL:TermList, T2:Term, RHO:Map, Heap:Map) =>
       dischargeTermApp(TL, [T2 dischargeTerm(T1, RHO, Heap)], RHO, Heap)

  rule dischargeTerm((delay T:Term), RHO:Map, Heap:Map) => (delay dischargeTerm(T, RHO, Heap))

  rule dischargeTerm((force T:Term), RHO:Map, Heap:Map) => (force dischargeTerm(T, RHO, Heap))
  
  rule dischargeTerm((error), _, _) => (error)

  rule dischargeTerm(T:Term, .Map, _) => T

endmodule
```
