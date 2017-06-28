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
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInt:Name       ( I1:Int I2:Int ) ) => I1 +Int I2
  rule ( builtin subtractInt:Name  ( I1:Int I2:Int ) ) => I1 -Int I2
  rule ( builtin multiplyInt:Name  ( I1:Int I2:Int ) ) => I1 *Int I2
  rule ( builtin divideInt:Name    ( I1:Int I2:Int ) ) => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInt:Name ( I1:Int I2:Int ) ) => I1 %Int I2
    when I2 =/=Int 0
  rule ( builtin lessThanInt:Name  ( I1:Int I2:Int ) ) => I1 <Int I2
  rule ( builtin equalsInt:Name    ( I1:Int I2:Int ) ) => I1 ==Int I2
  // TODO: Figure out _why_ 53 and 11.
  rule ( builtin intToFloat:Name   ( I:Int ) ) => Int2Float(I, 53, 11)

  rule ( builtin addFloat:Name      ( F1:Float F2:Float ) ) => F1 +Float F2
  rule ( builtin subtractFloat:Name ( F1:Float F2:Float ) ) => F1 -Float F2
  rule ( builtin multiplyFloat:Name ( F1:Float F2:Float ) ) => F1 *Float F2
  rule ( builtin divideFloat:Name   ( F1:Float F2:Float ) ) => F1 /Float F2
  rule ( builtin lessThanFloat:Name ( F1:Float F2:Float ) ) => F1 <Float F2
  rule ( builtin equalsFloat:Name   ( F1:Float F2:Float ) ) => F1 ==Float F2

  rule ( builtin ceil:Name  ( F:Float ) ) => #tmHolder(ceilFloat(F))
  rule ( builtin floor:Name ( F:Float ) ) => #tmHolder(floorFloat(F))
  rule #tmHolder(F:Float) => F
  rule ( builtin round:Name ( F:Float ) )
    => Float2Int(roundFloat(F:Float, 2, 2))

  syntax ByStr ::= toByStrVerbatim ( String )                        [function]
  rule toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule toString(B:ByStr):String
    => substrString(toStringVerbatim(B), 1, lengthString(toStringVerbatim(B)))

  rule ( builtin take:Name ( N:Int B:ByStr ) )
    => toByStr(substrString(toString(B), 0, N))

  rule ( builtin drop:Name ( N:Int B:ByStr ) )
    => toByStr(substrString(toString(B), N, lengthString(toString(B))))

  rule ( builtin concatenate:Name ( B1:ByStr B2:ByStr ) )
    => toByStr(toString(B1) +String toString(B2))

  rule ( builtin sha3_256:Name ( B1:ByStr ) )
    => toByStr(Keccak256(toString(B1)))

  /***************************************************************************/
  /************************   LOCAL REDUCTIONS   *****************************/
  /***************************************************************************/

  rule [ ( lam X M ) (N:Tm) ]  => M[N / X]
  rule ( let N:Tm X:Var M:Tm ):Tm => M[N / X]

  rule D:Def DS:DefList => D ~> DS

  rule
    <k> (( define N:Name V:Val ) => .K) ~> _ </k>
    <env> E => E[N <- V] </env>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS )
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
