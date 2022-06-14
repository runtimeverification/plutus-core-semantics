# UPLC Pretty-Print

```k
requires "uplc-syntax.md"

module UPLC-DISCHARGE
  imports INT
  imports LIST
  imports UPLC-SYNTAX

  syntax Term ::= dischargeTerm(Term, Map) [function]
  syntax Term ::= dischargeTermApp(TermList, Term, Map) [function]
  syntax Term ::= discharge(Value) [function]
  syntax Term ::= dischargeApp(BuiltinName, List) [function]
  syntax Term ::= dischargeAppAux(Term, List) [function]
  
  rule discharge(< con T:TypeConstant C:Constant >) => (con T C)  

  rule discharge(< lam I:UplcId T:Term RHO:Map >) => (lam I dischargeTerm(T, RHO)) 

  rule discharge(< delay T:Term RHO:Map >) => (delay dischargeTerm(T, RHO)) 

  rule discharge(< builtin BN:BuiltinName L:List _ >) =>
       dischargeApp(BN, L)

  rule dischargeApp(BN:PolyBuiltinName, ListItem(V:Value) L) =>
       dischargeAppAux([(force (builtin BN)) discharge(V)], L)

  rule dischargeApp(BN:BuiltinName, ListItem(V:Value) L) =>
       dischargeAppAux([(builtin BN) discharge(V)], L) [owise]

  rule dischargeAppAux(T, .List) => T
  rule dischargeAppAux(T, ListItem(V:Value) L:List) =>
       dischargeAppAux([T discharge(V)], L)

  rule dischargeTerm(X:UplcId, RHO) => discharge({RHO[X]}:>Value)
  requires X in_keys(RHO) 
  
  rule dischargeTerm(X:UplcId, RHO) => X
  requires notBool(X in_keys(RHO))

  rule dischargeTerm((con T:TypeConstant C:Constant), _) => (con T C)

  rule dischargeTerm((builtin BN:BuiltinName), _) => (builtin BN)

  rule dischargeTerm((lam X:UplcId T:Term), RHO:Map) => (lam X dischargeTerm(T, RHO))

  rule dischargeTerm([ T1:Term (T2:Term TL:TermList) ], RHO:Map) =>
       dischargeTermApp(TL, [dischargeTerm(T1, RHO) dischargeTerm(T2, RHO) ], RHO)

  rule dischargeTermApp(T1:Term TL:TermList, T2:Term, RHO:Map) =>
  
       dischargeTermApp(TL, [T2 dischargeTerm(T1, RHO)], RHO)

  rule dischargeTerm((delay T:Term), RHO:Map) => (delay dischargeTerm(T, RHO))

  rule dischargeTerm((force T:Term), RHO:Map) => (force dischargeTerm(T, RHO))
  
  rule dischargeTerm((error), _) => (error)

  rule dischargeTerm( T, .Map) => T

endmodule
```
