# UPLC Pretty-Print

```k
requires "uplc-syntax.md"
requires "uplc-configuration.md"

module UPLC-DISCHARGE
  imports INT
  imports LIST
  imports UPLC-SYNTAX
  imports UPLC-CONFIGURATION

  syntax Term ::= dischargeTerm(Term, Int) [function]
  syntax Term ::= dischargeTermApp(TermList, Term, Int) [function]
  syntax Term ::= discharge(Value) [function]
  syntax Term ::= dischargeApp(BuiltinName, List) [function]
  syntax Term ::= dischargeAppAux(Term, List) [function]
  
  rule discharge(< con T:TypeConstant C:Constant >) => (con T C)  

  rule discharge(< lam I:UplcId T:Term ID:Int >) => (lam I dischargeTerm(T, ID)) 

  rule discharge(< delay T:Term ID:Int >) => (delay dischargeTerm(T, ID)) 

  rule discharge(< builtin BN:BuiltinName L:List _ >) =>
       dischargeApp(BN, L)

  rule dischargeApp(BN:PolyBuiltinName, ListItem(V:Value) L) =>
       dischargeAppAux([(force (builtin BN)) discharge(V)], L)

  rule dischargeApp(BN:BuiltinName, ListItem(V:Value) L) =>
       dischargeAppAux([(builtin BN) discharge(V)], L) [owise]

  rule dischargeAppAux(T, .List) => T
  rule dischargeAppAux(T, ListItem(V:Value) L:List) =>
       dischargeAppAux([T discharge(V)], L)

  rule [[ dischargeTerm(X:UplcId, ID) => discharge({RHO[X]}:>Value) ]]
       <envID> ID </envID>
       <mappings> RHO </mappings>
  requires X in_keys(RHO) 
  
  rule [[ dischargeTerm(X:UplcId, ID) => X ]]
       <envID> ID </envID>
       <mappings> RHO </mappings>
  requires notBool(X in_keys(RHO))

  rule dischargeTerm((con T:TypeConstant C:Constant), _) => (con T C)

  rule dischargeTerm((builtin BN:BuiltinName), _) => (builtin BN)

  rule dischargeTerm((lam X:UplcId T:Term), ID) => (lam X dischargeTerm(T, ID))

  rule dischargeTerm([ T1:Term (T2:Term TL:TermList) ], ID) =>
       dischargeTermApp(TL, [dischargeTerm(T1, ID) dischargeTerm(T2, ID) ], ID)

  rule dischargeTermApp(T1:Term TL:TermList, T2:Term, ID) =>
  
       dischargeTermApp(TL, [T2 dischargeTerm(T1, ID)], ID)

  rule dischargeTerm((delay T:Term), ID) => (delay dischargeTerm(T, ID))

  rule dischargeTerm((force T:Term), ID) => (force dischargeTerm(T, ID))
  
  rule dischargeTerm((error), _) => (error)

  rule dischargeTerm( T, 0) => T

endmodule
```
