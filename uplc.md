```k
requires "domains.md"

module UPLC-SYNTAX
   imports UNSIGNED-INT-SYNTAX
   imports ID
   imports LIST
   imports BOOL

   syntax TypeConstant ::= "integer"
                         | "data"
                         | "bytestring"
                         | "unit"
                         | "bool"
                         
   syntax ByteString   ::= r"#([a-fA-F0-9][a-fA-F0-9])+" [token]
   
   syntax Constant     ::= Int
                         | "True"
                         | "False"
                         | ByteString
                         | "()"
                         
   syntax BuiltinName  ::= "addInteger"
                         | "multiplyInteger"
                         | "subtractInteger"
                         | "divideInteger"
                         | "lessThanInteger"
                         | "sha3_256"

   syntax Value ::= "(" "con" TypeConstant Constant ")" 
                  | "(" "lam" Id Term ")"              
                  | "(" "delay" Term ")"                
                  | "#SUM"
                  | #SUM(Value)
                  | #SUM(Value, Value)
                  | "#MUL"
                  | #MUL(Value)
                  | #MUL(Value, Value)
                  | "#SUB"
                  | #SUB(Value)
                  | #SUB(Value, Value)
                  | "#DIV"
                  | #DIV(Value)
                  | #DIV(Value, Value)
                  | "#LTI"
                  | #LTI(Value)
                  | #LTI(Value, Value)
                  

   syntax TermList ::= NeList{Term, ""}

   syntax Term ::= Id
                 | Value 
                 | "[" Term Term "]"                     // function application
                 | "(" "force" Term ")"                  // force execution of a term
                 | "(" "builtin" BuiltinName ")"
                 | "(" "builtin" BuiltinName TermList ")"// builtin
                 | "(" "error" ")"                       // error
                 
   syntax Version ::= r"[0-9]+.[0-9]+.[0-9]+" [token]

   syntax Program ::= "(" "program" Version Term ")"     // versioned program
endmodule

module UPLC-SEMANTICS
  imports UPLC-SYNTAX
  imports MAP
  imports INT
  imports K-EQUAL

  syntax AClosure ::= Clos(Value, Map)

  syntax ATerm ::= "Force"
                 | "[_" Term "]"
                 | "[" AClosure "_]"
                 | Term

  configuration <k> $PGM:Program </k>
                <env> .Map </env>
                <stack> .List </stack>

  rule <k> (program _V M) => M </k>

  rule <k> X:Id => V ... </k>
       <env> (_RHO:Map X |-> Clos(V, RHO')) => RHO' </env>

  rule <k> (force M:Term) => (M ~> Force) ... </k>

  rule <k> [ M N ] => M ~> [_ N] ... </k>

  rule <k> V:Value ~> [_ N] => N ~> [ Clos(V, RHO) _] ... </k>
       <env> RHO </env>

  rule <k> (V:Value ~> ([ Clos((lam X:Id M:Term), RHO') _] )) => M ... </k>
        <env> RHO => (RHO' (X |-> Clos(V, RHO))) </env>

   rule <k> (delay M:Term) ~> Force => M ... </k>

  // Builtins
  rule <k> (builtin BN (M Ms)) => M ~> (builtin BN Ms) ... </k>  
  rule <k> V:Value ~> (builtin BN Ms) => (builtin BN Ms) ... </k>
       <stack> ... (.List => ListItem(V)) </stack>

  // addInteger
  rule <k> (builtin addInteger .TermList) => (con integer I1 +Int I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin addInteger) => #SUM ... </k>
  rule <k> (V:Value ~> ([ Clos(#SUM, _RHO) _])) => #SUM(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#SUM(V2:Value), _RHO) _])) => #SUM(V1, V2) ... </k>
  rule <k> #SUM((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 +Int I2) ... </k>

  // multiplyInteger
  rule <k> (builtin multiplyInteger .TermList) => (con integer I1 *Int I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin multiplyInteger) => #MUL ... </k>
  rule <k> (V:Value ~> ([ Clos(#MUL, _RHO) _])) => #MUL(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#MUL(V2:Value), _RHO) _])) => #MUL(V1, V2) ... </k>
  rule <k> #MUL((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 *Int I2) ... </k>

  // subtractInteger
  rule <k> (builtin subtractInteger .TermList) => (con integer I1 -Int I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin subtractInteger) => #SUB ... </k>
  rule <k> (V:Value ~> ([ Clos(#SUB, _RHO) _])) => #SUB(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#SUB(V2:Value), _RHO) _])) => #SUB(V1, V2) ... </k>
  rule <k> #SUB((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 -Int I2) ... </k>

  // divideInteger
  rule <k> (builtin divideInteger .TermList) => (con integer I1 /Int I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin divideInteger) => #DIV ... </k>
  rule <k> (V:Value ~> ([ Clos(#DIV, _RHO) _])) => #DIV(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#DIV(V2:Value), _RHO) _])) => #DIV(V1, V2) ... </k>
  rule <k> #DIV((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 /Int I2) ... </k>

  // lessThanInteger
  rule <k> (builtin lessThanInteger .TermList) =>
           #if I1 <Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin lessThanInteger) => #LTI ... </k>
  rule <k> (V:Value ~> ([ Clos(#LTI, _RHO) _])) => #LTI(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#LTI(V2:Value), _RHO) _])) => #LTI(V1, V2) ... </k>

rule <k> #LTI((con integer I1:Int), (con integer I2:Int)) =>
         (#if I1 <Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

endmodule

module UPLC
     imports UPLC-SYNTAX
     imports UPLC-SEMANTICS
endmodule
```
