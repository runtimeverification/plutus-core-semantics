K Semantics of UPLC
===================

**UNDER CONSTRUCTION**

```k
requires "domains.md"

module PLUTUS-CORE-LEXICAL-GRAMMAR
   imports UNSIGNED-INT-SYNTAX
   syntax Name         ::= r"[a-zA-Z][a-zA-Z0-9\\_\\']*" [token] // name
   syntax Var          ::= Name                                  // term variable
   syntax TyVar        ::= Name                                  // type variable
   syntax BuiltinName  ::= Name                                  // builtin term name
// syntax Integer      ::= r"[+-]?[0-9]+"             [token]    // int
   syntax ByteString   ::= r"#([a-fA-F0-9][a-fA-F0-9])+" [token] // hex string
   syntax Version      ::= r"[0-9]+(.[0-9]+)*"           [token] // version
   syntax Constant     ::= "()"                                  // unit constant
                         | "True" | "False"                      // boolean constant
//                       | Integer                               // integer constant
// Todo: We are using builtin Int from UNSIGNED-INT-SYNTAX to avoid and ambiguity
// probably between Integer and Version.
                         | Int
                         | ByteString                            // bytestring constant
   syntax TypeConstant ::= Name                                  // type constant
endmodule

module UNTYPED-PLUTUS-CORE-GRAMMAR
   imports PLUTUS-CORE-LEXICAL-GRAMMAR
   imports LIST

   syntax Value ::= "(" "con" TypeConstant Constant ")"   // constant
                  | "(" "lam" Var Term ")"                // lambda abstraction
                  | "(" "delay" Term ")"                  // delay execution of a term

   syntax Term ::= Var
                 | Value 
                 | "[" Term Term "]"                        // function application
                 | "(" "force" Term ")"                     // force execution of a term
                 | "(" "builtin" BuiltinName List ")"       // builtin
                 | "(" "error" ")"                          // error

   syntax Program ::= "(" "program" Version Term ")"        // versioned program
endmodule

module UNTYPED-PLUTUS-CORE-CEK
  imports UNTYPED-PLUTUS-CORE-GRAMMAR
  imports MAP
  imports LIST
  imports INT

  syntax AClosure ::= Clos(Value, Map)
  syntax AFrame   ::= "[" "_" Term "]"
                    | "[" AClosure "_" "]"
                    | BuiltinApp(BuiltinName, List, List, Map)
                    | "Force"

  syntax TypeConstant ::= "integer" [token]

  syntax BuiltinName ::= "addInteger" [token]

  configuration <k> $PGM:Program </k>
                <env> .Map </env>
		<stack> .List </stack>
  
  // <k> error </> <env> RHO </env> <stack> S </stack>
  // is the error state.
 
  // <k> V </> <env> RHO </env> <stack> .Map </stack>
  // is a final state.

  rule <k> (program _V M) => M ... </k>

  rule <k> X:Var => V ... </k>
       <env> (_RHO X |-> Clos(V, RHO')) => RHO' </env>

  // s ; \rho |> (con tn cn) |-> s ; \rho <| (con tn cn)
  // s ; \rho |> (lam x M)   |-> s ; \rho <| (lam x M)
  // s ; \rho |> (delay M)   |-> s ; \rho <| (delay M)

  rule <k> (force M:Term) => M ... </k>
       <stack> ... (.List => ListItem(Force)) </stack>

  rule <k> [ M N ] => M ... </k>
       <stack> ... (.List => ListItem([_ N])) </stack>

  // s ; \rho |> (builtin bn) |-> s ; \rho |> M (bn computes to M)

  rule <k> (builtin BN:BuiltinName ( ListItem( M:Term ) Ms:List ) ) => M ... </k>
       <env> RHO </env>
       <stack> ... (.List => ListItem(BuiltinApp(BN, .List, Ms, RHO))) </stack>

  // s ; \rho |> error |-> <>

  // s ; \rho <| V |-> [](V, \rho)
  
  rule <k> V:Value => N ... </k>
       <env> RHO </env>
       <stack> ... (ListItem([_ N:Term]) =>
                    ListItem([ Clos(V, RHO) _])) </stack>

  rule <k> V:Value => M ... </k>
       <env> RHO => RHO' (X |-> Clos(V, RHO)) </env>
       <stack> ... (ListItem([ Clos((lam X:Var M), RHO') _]) => .List) </stack>
       
  rule <k> (delay M:Term) => M </k>
       <stack> ... (ListItem(Force) => .List) </stack>

  // s , ((builtin bn Cs _ M Ms), \rho') ; \rho <| V    |->
  // s , ((builtin bn Cs(V, \rho) _ Ms), \rho') ; \rho' |> M
  rule <k> V:Value => M </k>
       <env> RHO => RHO' </env>
       <stack> ... (ListItem(BuiltinApp(BN, C, (ListItem(M) Ms), RHO')) =>
                    ListItem(BuiltinApp(BN, (C ListItem(Clos(V, RHO))), Ms, RHO')))
       </stack>

  // s , ((builtin bn Cs _), \rho') ; \rho <| V  |->
  // s ; \rho' |> M  (bn computes on Cs (V, \rho) to M)

  // rule <k> (con int I2) => (con int (I1 +Int I2)) </k>
  //      <env> _ => RHO' </env>
  //      <stack> ... (ListItem(BuiltinApp(addInteger, ListItem(Clos((con int I1:Int), _RHO)), .List, RHO')) => .List) </stack>
endmodule

module UPLC
    imports UNTYPED-PLUTUS-CORE-GRAMMAR
    imports UNTYPED-PLUTUS-CORE-CEK
endmodule
```
