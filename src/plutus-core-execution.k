require "plutus-core-syntax.k"
require "substitution.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports SUBSTITUTION
  imports KRYPTO

  configuration
    <T>
      <k> $PGM:Mod </k>
      <env> .Map </env>
      <mod> .ModName </mod>
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInt:Name       I1:Int I2:Int ) => I1 +Int I2
  rule ( builtin subtractInt:Name  I1:Int I2:Int ) => I1 -Int I2
  rule ( builtin multiplyInt:Name  I1:Int I2:Int ) => I1 *Int I2
  rule ( builtin divideInt:Name    I1:Int I2:Int ) => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInt:Name I1:Int I2:Int ) => I1 %Int I2
    when I2 =/=Int 0

  syntax Tm ::= #conHolder(K, K)
              | #qualCHolder(K, K)
  rule #conHolder(MN, C) => ( con (MN . C) .ValList )

  syntax Prim ::= "#primTrue" | "#primFalse"
  rule #primTrue => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                              #parseToken("Con@PLUTUS-CORE-SYNTAX", "True"))
  rule #primFalse => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                              #parseToken("Con@PLUTUS-CORE-SYNTAX", "False"))

  rule ( builtin equalsInt:Name I:Int I:Int ) => #primTrue
  rule ( builtin equalsInt:Name I1:Int I2:Int ) => #primFalse
    when I1 =/=Int I2

  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #primTrue
    when I1 <Int I2
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #primFalse
    when notBool (I1 <Int I2)

  // TODO: Figure out _why_ 53 and 11.
  rule ( builtin intToFloat:Name   I:Int ) => Int2Float(I, 53, 11)

  rule ( builtin intToByteString:Name 0 )    => toByStr("0")
  rule ( builtin intToByteString:Name I:Int) => toByStr(#toHex(I))
    when I =/=Int 0

  rule ( builtin addFloat:Name      F1:Float F2:Float ) => F1 +Float F2
  rule ( builtin subtractFloat:Name F1:Float F2:Float ) => F1 -Float F2
  rule ( builtin multiplyFloat:Name F1:Float F2:Float ) => F1 *Float F2
  rule ( builtin divideFloat:Name   F1:Float F2:Float ) => F1 /Float F2

  rule ( builtin lessThanFloat:Name F1:Float F2:Float ) => #primTrue
    when F1 <Float F2
  rule ( builtin lessThanFloat:Name F1:Float F2:Float ) => #primFalse
    when notBool (F1 <Float F2)
  rule ( builtin equalsFloat:Name   F1:Float F2:Float ) => #primTrue
    when F1 ==Float F2
  rule ( builtin equalsFloat:Name   F1:Float F2:Float ) => #primFalse
    when F1 =/=Float F2

  syntax String ::= #toHex(Int)                                      [function]
                  | #hex(Int)                                        [function]

  rule #hex(0)  => "0"
  rule #hex(1)  => "1"
  rule #hex(2)  => "2"
  rule #hex(3)  => "3"
  rule #hex(4)  => "4"
  rule #hex(5)  => "5"
  rule #hex(6)  => "6"
  rule #hex(7)  => "7"
  rule #hex(8)  => "8"
  rule #hex(9)  => "9"
  rule #hex(10) => "A"
  rule #hex(11) => "B"
  rule #hex(12) => "C"
  rule #hex(13) => "D"
  rule #hex(14) => "E"
  rule #hex(15) => "F"

  // This will fail on 0, so should not be used by itself. See the rules for
  // `(builtin intToByteString (...))`.
  rule #toHex(0) => ""
  rule #toHex(N:Int) => #toHex(N /Int 16) +String #hex(N %Int 16)
    when N =/=Int 0

  rule ( builtin ceil:Name  F:Float ) => #tmHolder(ceilFloat(F))
  rule ( builtin floor:Name F:Float ) => #tmHolder(floorFloat(F))
  rule #tmHolder(F:Float) => F
  rule ( builtin round:Name F:Float )
    => Float2Int(roundFloat(F:Float, 2, 2))

  syntax ByStr ::= toByStrVerbatim ( String )                        [function]
  rule toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule toString(B:ByStr):String
    => substrString(toStringVerbatim(B), 1, lengthString(toStringVerbatim(B)))

  rule ( builtin take:Name N:Int B:ByStr )
    => toByStr(substrString(toString(B), 0, N))

  rule ( builtin drop:Name N:Int B:ByStr )
    => toByStr(substrString(toString(B), N, lengthString(toString(B))))

  rule ( builtin concatenate:Name B1:ByStr B2:ByStr )
    => toByStr(toString(B1) +String toString(B2))

  rule ( builtin sha3_256:Name B:ByStr ) => toByStr(Sha3256(toString(B)))
  rule ( builtin sha2_256:Name B:ByStr ) => toByStr(Sha256(toString(B)))

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primTrue
    when toString(B1) ==String toString(B2)
  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primFalse
    when toString(B1) =/=String toString(B2)

  /***************************************************************************/
  /************************   LOCAL REDUCTIONS   *****************************/
  /***************************************************************************/

  rule [ ( lam X M ) (N:Tm) ]  => M[N / X]
  rule ( let N:Tm X:Var M:Tm ):Tm => M[N / X]

  rule D:Def DS:DefList => D ~> DS

  rule #qualN(MN:ModName, N:Name) => (MN . N):QualN

  rule
    <k> (( define N:Name V:Val ) => .K) ~> _ </k>
    <mod> MN </mod>
    <env> E => E[#qualN(MN, N) <- V] </env>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k> ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS ) </k>
    <mod> CurrMN => MN </mod>

  rule
    <k> QN:QualN => V ... </k>
    <env> ... QN |-> V:Val ... </env>

  rule (.DefList ~> _:ModF ) => .K

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax Map ::= manySubst(VarList, ValList) [function]
  rule manySubst(.VarList, .ValList) => .Map
  rule manySubst(X:Var XS:VarList, V:Val VS:ValList)
    => manySubst(XS, VS)[X <- V]

  syntax K ::= substTerm(Tm, Map)

  rule ( case ( con QC:QualC MS:ValList ) ( cl QC ( XS:VarList ) M:Tm ) _:ClList )
    => substTerm(M, manySubst(XS, MS))
  rule ( case ( con QC:QualC MS:ValList ) ( cl QC':QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => ( case ( con QC:QualC MS:ValList ) CL )
    when QC =/=K QC'

  rule substTerm(T:Tm, M:Map) => T[M]

endmodule
