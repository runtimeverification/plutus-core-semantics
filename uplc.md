```k
requires "domains.md"
requires "krypto.md"

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
                         
   syntax BuiltinName  ::= "ifThenElse"
                         | "addInteger"
                         | "subtractInteger"
                         | "multiplyInteger"
                         | "divideInteger"
                         | "modInteger"
                         | "quotientInteger"
                         | "remainderInteger"
                         | "lessThanInteger"
                         | "lessThanEqualsInteger"
                         | "greaterThanInteger"
                         | "greaterThanEqualsInteger"
                         | "equalsInteger"
                         | "sha3_256"

   syntax Value ::= "(" "con" TypeConstant Constant ")" 
                  | "(" "lam" Id Term ")"              
                  | "(" "delay" Term ")"
                  | "#ITE"
                  | #ITE(Value)
                  | #ITE(Value, Value)
                  | #ITE(Value, Value, Value)
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
                  | "#MOD" 
                  | #MOD(Value)
                  | #MOD(Value, Value)
                  | "#QUO" 
                  | #QUO(Value)
                  | #QUO(Value, Value)
                  | "#REM" 
                  | #REM(Value)
                  | #REM(Value, Value)
                  | "#LTI"
                  | #LTI(Value)
                  | #LTI(Value, Value)
                  | "#LTE"
                  | #LTE(Value)
                  | #LTE(Value, Value)
                  | "#GTI"
                  | #GTI(Value)
                  | #GTI(Value, Value)
                  | "#GTE"
                  | #GTE(Value)
                  | #GTE(Value, Value)
                  | "#EQI" 
                  | #EQI(Value)
                  | #EQI(Value, Value)
                  

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
  imports KRYPTO

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

  // ifThenElse
  rule <k> (builtin ifThenElse .TermList) => V1 </k>
       <stack> ... (ListItem((con bool True))
                    ListItem(V1:Value)
                    ListItem(_V2:Value)) => .List </stack>

  rule <k> (builtin ifThenElse .TermList) => V2 </k>
       <stack> ... ((ListItem((con bool False))
                     ListItem(_V1:Value)
                     ListItem(V2:Value)) => .List) </stack>

  rule <k> (builtin ifThenElse) => #ITE ... </k>
  
  rule <k> (V:Value ~> ([ Clos(#ITE, _RHO) _])) => #ITE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ITE(V1:Value), _RHO) _])) => #ITE(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#ITE(V1:Value, V2:Value), _RHO) _])) => #ITE(V1, V2, V3) ... </k>

  rule <k> #ITE((con bool True), V1:Value, _V2:Value) => V1 </k>

  rule <k> #ITE((con bool False), _V1:Value, V2:Value) => V2 </k>

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

  // divideInteger: According to Plutus specification,
  // divideInteger implements standard mathematical integer division operation.

  rule <k> (builtin divideInteger .TermList) => (con integer I1 divInt I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 =/=Int 0

  rule <k> (builtin divideInteger .TermList) => (error) ... </k>
       <stack> ... (ListItem((con integer _I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 ==Int 0

  rule <k> (builtin divideInteger) => #DIV ... </k>

  rule <k> (V:Value ~> ([ Clos(#DIV, _RHO) _])) => #DIV(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#DIV(V1:Value), _RHO) _])) => #DIV(V1, V2) ... </k>

  rule <k> #DIV((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 divInt I2) ... </k>
  requires I2 =/=Int 0
  
  rule <k> #DIV((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0

  // modInteger: According to Plutus specification,
  // modInteger implements standard mathematical integer division operation.

  rule <k> (builtin modInteger .TermList) => (con integer I1 modInt I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 =/=Int 0

  rule <k> (builtin modInteger .TermList) => (error) ... </k>
       <stack> ... (ListItem((con integer _I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 ==Int 0

  rule <k> (builtin modInteger) => #MOD ... </k>

  rule <k> (V:Value ~> ([ Clos(#MOD, _RHO) _])) => #MOD(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#MOD(V1:Value), _RHO) _])) => #MOD(V1, V2) ... </k>

  rule <k> #MOD((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 modInt I2) ... </k>
  requires I2 =/=Int 0
  
  rule <k> #DIV((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0

  // quotientInteger: According to Plutus specification, quotientInteger rounds towards 0.
  // According to
  // https://github.com/kframework/k/blob/master/k-distribution/include/kframework/builtin/domains.md
  // Operator /Int computes the quotient using t-division which rounds towards 0. 

  rule <k> (builtin quotientInteger .TermList) => (con integer I1 /Int I2) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 =/=Int 0

  rule <k> (builtin quotientInteger .TermList) => (error) ... </k>
       <stack> ... (ListItem((con integer _I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 ==Int 0

  rule <k> (builtin quotientInteger) => #QUO ... </k>

  rule <k> (V:Value ~> ([ Clos(#QUO, _RHO) _])) => #QUO(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#QUO(V1:Value), _RHO) _])) => #QUO(V1, V2) ... </k>

  rule <k> #QUO((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 /Int I2) ... </k>
  requires I2 =/=Int 0
  
  rule <k> #QUO((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0

  // remainderInteger cooresponds to Haskell rem, according to Plutus specification.
  // From Haskell documentation:
  // rem is integer remainder, satisfying:
  // (x `quot` y)*y + (x `rem` y) == x

  rule <k> (builtin remainderInteger .TermList) =>
           (con integer (I1 -Int (I1 /Int I2) *Int I2)) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 =/=Int 0

  rule <k> (builtin remainderInteger .TermList) => (error) ... </k>
       <stack> ... (ListItem((con integer _I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>
  requires I2 ==Int 0

  rule <k> (builtin remainderInteger) => #REM ... </k>

  rule <k> (V:Value ~> ([ Clos(#REM, _RHO) _])) => #REM(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#REM(V1:Value), _RHO) _])) => #REM(V1, V2) ... </k>

  rule <k> #REM((con integer I1:Int), (con integer I2:Int)) =>
           (con integer (I1 -Int (I1 /Int I2) *Int I2)) ... </k>
  requires I2 =/=Int 0
  
  rule <k> #REM((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0

  // lessThanInteger
  rule <k> (builtin lessThanInteger .TermList) =>
           #if I1 <Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin lessThanInteger) => #LTI ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTI, _RHO) _])) => #LTI(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTI(V1:Value), _RHO) _])) => #LTI(V1, V2) ... </k>

  rule <k> #LTI((con integer I1:Int), (con integer I2:Int)) =>
           (#if I1 <Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

  // lessThanEqualsInteger
  rule <k> (builtin lessThanEqualsInteger .TermList) =>
           #if I1 <=Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin lessThanEqualsInteger) => #LTE ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTE, _RHO) _])) => #LTE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTE(V1:Value), _RHO) _])) => #LTE(V1, V2) ... </k>

  rule <k> #LTE((con integer I1:Int), (con integer I2:Int)) =>
           (#if I1 <=Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

  // greaterThanInteger
  rule <k> (builtin greaterThanInteger .TermList) =>
           #if I1 >Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin greaterThanInteger) => #GTI ... </k>
  
  rule <k> (V:Value ~> ([ Clos(#GTI, _RHO) _])) => #GTI(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#GTI(V1:Value), _RHO) _])) => #GTI(V1, V2) ... </k>

  rule <k> #GTI((con integer I1:Int), (con integer I2:Int)) =>
           (#if I1 >Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

  // greaterThanEqualsInteger
  rule <k> (builtin lessThanEqualsInteger .TermList) =>
           #if I1 >=Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin greaterThanEqualsInteger) => #GTE ... </k>
  rule <k> (V:Value ~> ([ Clos(#GTE, _RHO) _])) => #GTE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#GTE(V1:Value), _RHO) _])) => #GTE(V1, V2) ... </k>

  rule <k> #GTE((con integer I1:Int), (con integer I2:Int)) =>
           (#if I1 >=Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

  // equalsInteger
  rule <k> (builtin equalsInteger .TermList) =>
           #if I1 ==Int I2 #then (con bool True) #else (con bool False) #fi ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int)) => .List) </stack>

  rule <k> (builtin equalsInteger) => #EQI ... </k>
  rule <k> (V:Value ~> ([ Clos(#EQI, _RHO) _])) => #EQI(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#EQI(V1:Value), _RHO) _])) => #EQI(V1, V2) ... </k>

  rule <k> #EQI((con integer I1:Int), (con integer I2:Int)) =>
           (#if I1 >=Int I2 #then (con bool True) #else (con bool False) #fi) ... </k>

endmodule

module UPLC
     imports UPLC-SYNTAX
     imports UPLC-SEMANTICS
endmodule
```
