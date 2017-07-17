require "plutus-core-syntax.k"
require "hex-conversion.k"
require "substitution.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports SUBSTITUTION
  imports KRYPTO
  imports HEX-CONVERSION

  configuration
    <T>
      <k> $PGM:Mod </k>
      <env>  .Map </env>
      <mod>  .ModName </mod>
      <benv> "TXH" |-> -1 "BN" |-> -1 "BT" |-> -1 </benv> // blockchain environment
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
  rule #primTrue
    => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                  #parseToken("Con@PLUTUS-CORE-SYNTAX", "True"))
  rule #primFalse
    => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                  #parseToken("Con@PLUTUS-CORE-SYNTAX", "False"))

  rule ( builtin equalsInt:Name I:Int I:Int     ) => #primTrue
  rule ( builtin equalsInt:Name I1:Int I2:Int   ) => #primFalse
                                                       when I1 =/=Int I2
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #primTrue
                                                       when I1 <Int I2
  rule ( builtin lessThanInt:Name I1:Int I2:Int )
    => #primFalse when notBool (I1 <Int I2)

  // TODO: Figure out _why_ 53 and 11.
  rule ( builtin intToFloat:Name   I:Int ) => #toPlFloat(Int2Float(I, 53, 11))

  rule ( builtin intToByteString:Name 0 )    => #toByStr("0")
  rule ( builtin intToByteString:Name I:Int) => #toByStr(#toHex(I))
    when I =/=Int 0

  rule ( builtin addFloat:Name      F1:PlFloat F2:PlFloat )
    => #toPlFloat(#toFloat(F1) +Float #toFloat(F2))
  rule ( builtin subtractFloat:Name F1:PlFloat F2:PlFloat )
    => #toPlFloat(#toFloat(F1) -Float #toFloat(F2))
  rule ( builtin multiplyFloat:Name F1:PlFloat F2:PlFloat )
    => #toPlFloat(#toFloat(F1) *Float #toFloat(F2))
  rule ( builtin divideFloat:Name   F1:PlFloat F2:PlFloat )
    => #toPlFloat(#toFloat(F1) /Float #toFloat(F2))
  rule ( builtin lessThanFloat:Name F1:PlFloat F2:PlFloat )
    => #primTrue when #toFloat(F1) <Float #toFloat(F2)
  rule ( builtin lessThanFloat:Name F1:PlFloat F2:PlFloat )
    => #primFalse when notBool (#toFloat(F1) <Float #toFloat(F2))
  rule ( builtin equalsFloat:Name   F1:PlFloat F2:PlFloat )
    => #primTrue when #toFloat(F1) ==Float #toFloat(F2)
  rule ( builtin equalsFloat:Name   F1:PlFloat F2:PlFloat )
    => #primFalse when #toFloat(F1) =/=Float #toFloat(F2)


  rule ( builtin ceil:Name  F:PlFloat ) => #toPlFloat(ceilFloat(#toFloat(F)))
  rule ( builtin floor:Name F:PlFloat ) => #toPlFloat(floorFloat(#toFloat(F)))
  rule ( builtin round:Name F:PlFloat )
    => Float2Int(roundFloat(#toFloat(F), 2, 2))

  syntax PlFloat ::= #toPlFloat(Float)                                [function]
  rule #toPlFloat(F:Float)
    => #parseToken("PlFloat@PLUTUS-CORE-SYNTAX", Float2String(F))
  rule #toFloat(F:PlFloat)
    => #parseToken("Float@FLOAT-SYNTAX", #toStringFloat(F))

  syntax ByStr ::= #toByStr ( String )                                [function]
  rule #toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule #toString(B:ByStr):String
    => substrString(#toStringVerbatim(B), 1, lengthString(#toStringVerbatim(B)))

  rule ( builtin take:Name N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), 0, N))

  rule ( builtin drop:Name N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), N, lengthString(#toString(B))))

  rule ( builtin concatenate:Name B1:ByStr B2:ByStr )
    => #toByStr(#toString(B1) +String #toString(B2))

  rule ( builtin sha3_256:Name B:ByStr ) => #toByStr(Sha3256(#toString(B)))
  rule ( builtin sha2_256:Name B:ByStr ) => #toByStr(Sha256(#toString(B)))

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primTrue
    when #toString(B1) ==String #toString(B2)
  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primFalse
    when #toString(B1) =/=String #toString(B2)

  /***************************************************************************/
  /************************   LOCAL REDUCTIONS   *****************************/
  /***************************************************************************/

  syntax K ::= #appFreeze(TmList)                                      [prefer]

  // Explicit strictness for nested application
  rule [ [ F:Val M:Tm ] MS:TmList ] => [ F M ] ~> #appFreeze(MS)
  rule M:Val ~> #appFreeze(MS) => [ M MS ]
  rule [ F:Val M1:Tm M2:Tm MS:TmList ] => [ [ F M1 ] M2 MS ]

  // When a lambda abstracts more than one variable, desugar it to a
  // combination of unary lambdas.
  rule [ ( lam X1:Var X2:Var XS:VarList M:Tm ) N:Tm ]
    => [ ( lam X1 .VarList ( lam X2 XS M ) ) N ]
  // In the unary case, just perform the substitution.
  rule [ ( lam X:Var .VarList M:Tm ) N:Tm ] => M[N / X]

  rule ( let N:Tm X:Var M:Tm ):Tm => M[N / X]

  // Process the definitions in order.
  rule D:Def DS:DefList => D ~> DS

  rule #qualN(MN:ModName, N:Name) => (MN . N):QualN

  rule
    <k> (( define N:Name V:Val ) => .K) ~> _ </k>
    <mod> MN </mod>
    <env> E => E[#qualN(MN, N) <- V] </env>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS )
    </k>
    <mod> CurrMN => MN </mod>

  rule
    <k> QN:QualN => V ... </k>
    <env> ... QN |-> V:Val ... </env>

  rule (.DefList ~> _:ModF ) => .K

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax Map ::= #manySubst(VarList, ValList)                        [function]

  rule #manySubst(.VarList, .ValList) => .Map
  rule #manySubst(X:Var XS:VarList, V:Val VS:ValList)
    => #manySubst(XS, VS)[X <- V]

  syntax K ::= substTerm(Tm, Map)

  rule ( case ( con QC:QualC MS:ValList )
              ( cl QC ( XS:VarList ) M:Tm ) _:ClList )
    => substTerm(M, #manySubst(XS, MS))
  rule ( case ( con QC:QualC MS:ValList )
              ( cl QC':QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => ( case ( con QC:QualC MS:ValList ) CL ) when QC =/=K QC'

  rule substTerm(T:Tm, M:Map) => T[M]

  /***************************************************************************/
  /************************   BLOCKCHAIN, BIND   *****************************/
  /***************************************************************************/

  rule
    <k> ( txhash ):Instr => V ... </k>
    <benv> ... TXH |-> V ... </benv>

  rule
    <k> ( blocknum ):Instr => V ... </k>
    <benv> ... BN |-> V ... </benv>

  rule
    <k> ( blocktime ):Instr => V ... </k>
    <benv> ... BT |-> V ... </benv>

  rule ( bind V:Val X:Var M:Tm ):Instr => M[V/X]

endmodule
