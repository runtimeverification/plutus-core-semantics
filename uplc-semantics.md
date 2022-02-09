```k
require "domains.md"
require "krypto.md"
require "bytestring.md"

module UPLC-SEMANTICS
  imports UPLC-SYNTAX
  imports MAP
  imports INT
  imports K-EQUAL
  imports KRYPTO
  imports BYTESTRING

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

  rule <k> #MOD((con integer _I1:Int), (con integer I2:Int)) =>
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
  rule <k> (builtin greaterThanEqualsInteger .TermList) =>
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

  // appendByteString
  rule <k> (builtin appendByteString .TermList) =>
           (con bytestring #appendByteString(B1, B2)) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin appendByteString) => #ABS ... </k>

  rule <k> (V:Value ~> ([ Clos(#ABS, _RHO) _])) => #ABS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ABS(V1:Value), _RHO) _])) => #ABS(V1, V2) ... </k>

  rule <k> #ABS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bytestring #appendByteString(B1, B2)) ... </k>

  // consByteString
  rule <k> (builtin consByteString .TermList) =>
           (con bytestring #consByteString(I, B)) ... </k>
       <stack> ... (ListItem((con integer I:Int))
                    ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin consByteString) => #CBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#CBS, _RHO) _])) => #CBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CBS(V1:Value), _RHO) _])) => #CBS(V1, V2) ... </k>

  rule <k> #CBS((con integer I:Int), (con bytestring B:ByteString)) =>
           (con bytestring #consByteString(I, B)) ... </k>

  // sliceByteString
  rule <k> (builtin sliceByteString .TermList) =>
           (con bytestring #sliceByteString(I1, I2, B)) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int))
                    ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin sliceByteString) => #SBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#SBS, _RHO) _])) => #SBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#SBS(V1:Value), _RHO) _])) => #SBS(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#SBS(V1:Value, V2:Value), _RHO) _])) => #SBS(V1, V2, V3) ... </k>

  rule <k> #SBS((con integer I1:Int), (con integer I2:Int), (con bytestring B:ByteString)) =>
           (con bytestring #sliceByteString(I1, I2, B)) ... </k>

  // lengthOfByteString
  rule <k> (builtin lengthOfByteString .TermList) =>
           (con bytestring #lengthOfByteString(B)) ... </k>
       <stack> ... (ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin lengthOfByteString) => #LBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LBS, _RHO) _])) => #LBS(V) ... </k>

  rule <k> #LBS((con bytestring B:ByteString)) =>
           (con integer #lengthOfByteString(B)) ... </k>

  // indexByteString
  rule <k> (builtin indexByteString .TermList) =>
           (con bytestring #indexByteString(B, I)) ... </k>
       <stack> ... (ListItem((con bytestring B:ByteString))
                    ListItem((con integer I:Int)) => .List) </stack>

  rule <k> (builtin indexByteString) => #IBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#IBS, _RHO) _])) => #IBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#IBS(V1:Value), _RHO) _])) => #IBS(V1, V2) ... </k>

  rule <k> #IBS((con bytestring B:ByteString), (con integer I:Int)) =>
           (con integer #indexByteString(B, I)) ... </k>

  // equalsByteString
  rule <k> (builtin equalsByteString .TermList) =>
           (con bool
	    #if (#equalsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin equalsByteString) => #EBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#EBS, _RHO) _])) => #EBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#EBS(V1:Value), _RHO) _])) => #EBS(V1, V2) ... </k>

  rule <k> #EBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#equalsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>

  // lessThanByteString
  rule <k> (builtin lessThanByteString .TermList) =>
           (con bool
	    #if (#lessThanByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin lessThanByteString) => #LTBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTBS, _RHO) _])) => #LTBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTBS(V1:Value), _RHO) _])) => #LTBS(V1, V2) ... </k>

  rule <k> #LTBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#lessThanByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>

  // lessThanEqualsByteString
  rule <k> (builtin lessThanEqualsByteString .TermList) =>
           (con bool
	    #if (#lessThanEqualsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin lessThanEqualsByteString) => #LEBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LEBS, _RHO) _])) => #LEBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LEBS(V1:Value), _RHO) _])) => #LEBS(V1, V2) ... </k>

  rule <k> #LEBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#lessThanEqualsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
endmodule

```
