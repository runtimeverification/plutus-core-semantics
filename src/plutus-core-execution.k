require "plutus-core-syntax.k"
require "hex-conversion.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports KRYPTO
  imports HEX-CONVERSION

  configuration
    <T>
      <k> $PGM:Mod </k>
      <lenv> .Map </lenv>
      <genv> .Map </genv>
      <mod> .ModName </mod>
      <sym> .Map </sym>
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

  rule #stringToModName(S:String) => #parseToken("ModName@PLUTUS-CORE-SYNTAX", S)
  rule #stringToCon(S:String) => #parseToken("Con@PLUTUS-CORE-SYNTAX", S)

  syntax Prim ::= "#primTrue" | "#primFalse"
  rule #primTrue
    => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                  #parseToken("Con@PLUTUS-CORE-SYNTAX", "True"))
  rule #primFalse
    => #conHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Nat"),
                  #parseToken("Con@PLUTUS-CORE-SYNTAX", "False"))

  rule ( builtin equalsInt:Name I:Int I:Int     ) => #primTrue [transition]
  rule ( builtin equalsInt:Name I1:Int I2:Int   ) => #primFalse
                                                       when I1 =/=Int I2
                                                       [transition]
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

  // TODO: fix sha stuff to work with current K
  rule ( builtin sha3_256:Name B:ByStr ) => #toByStr(Sha3256(#toString(B)))
  rule ( builtin sha2_256:Name B:ByStr ) => #toByStr(Sha256(#toString(B)))

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primTrue
    when #toString(B1) ==String #toString(B2)
  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr ) => #primFalse
    when #toString(B1) =/=String #toString(B2)

  /***************************************************************************/
  /************************   LOCAL REDUCTIONS   *****************************/
  /***************************************************************************/

  // syntax K ::= #appFreeze(TmList)                                      [prefer]

  // Explicit strictness for nested application
  // rule [ [ F:Val M:Tm ] MS:TmList ] => [ F M ] ~> #appFreeze(MS)
  // rule M:Val ~> #appFreeze(MS) => [ M MS ]
  rule [ F:Val M1:Tm M2:Tm MS:TmList ] => [ [ F M1 ] M2 MS ]

  rule ( lam .VarList M:Tm ) => M
  rule [ V:Val .TmList ] => V

  // When a lambda abstracts more than one variable, desugar it to a
  // combination of unary lambdas.
  // rule [ ( lam X1:Var X2:Var XS:VarList M:Tm ) N:Tm ]
  //   => [ ( lam X1 .VarList ( lam X2 XS M ) ) N ]

  rule ( lam X1:Var X2:Var XS:VarList M:Tm )
    => ( lam X1 .VarList ( lam X2 XS M ) )

  // In the unary case, just perform the substitution.

  syntax Val ::= closure(Map, Var, Tm)
  
  syntax Tm ::= app(Tm, Tm) [strict]

  rule [ F:Val M:Tm ] => app(F, M)
  
  rule
    <k> ( lam X:Var M:Tm ):Tm => closure(Rho, X, M) ... </k>
    <lenv> Rho </lenv>
    [structural]

  rule
    <k> app(closure(Rho, X, M), V:Val) => M ~> Rho' ... </k>
    <lenv> Rho' => Rho[X <- V] </lenv>
    [structural]

  rule
    <k> _:Val ~> (Rho => .K) ... </k>
    <lenv> _ => Rho </lenv>
    [structural]

  rule ( let N:Tm X:Var M:Tm ):Tm => [ ( lam X M ) N ]

  // Process the definitions in order.
  rule D:Def DS:DefList => D ~> DS

  rule
    <k> ( define N:Name V:Val ) => .K ... </k>
    <mod> MN </mod>
    <lenv> _ => .Map </lenv>
    <genv> ... .Map => (MN . N):QualN |-> V ... </genv>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS )
    </k>
    <mod> CurrMN => MN </mod>

  rule
    <k> QN:QualN => V ... </k>
    <genv> ... QN |-> V ... </genv>

  rule
    <k> X:Var => V ... </k>
    <lenv> ... X |-> V ... </lenv>

  rule (.DefList ~> _:ModF ) => .K

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax TmList ::= #toTermList(ValList)                             [function]
  rule #toTermList(.ValList) => .TmList
  rule #toTermList(M:Val MS:ValList) => M #toTermList(MS)

  syntax Tm ::= "#ifTm" Bool "#then" Tm "#else" Tm "#fi"             [function]

  rule ( case ( con QC:QualC MS:ValList )
              ( cl QC':QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => #ifTm (QC ==K QC') #then [ ( lam XS M ) #toTermList(MS) ]
                          #else ( case ( con QC:QualC MS:ValList ) CL )
       #fi

  rule #ifTm true #then M #else _ #fi => M
  rule #ifTm false #then _ #else M #fi => M

  syntax K ::= #freezeCon(QualC, ValList)
             | "#freezeSucc()"

  rule ( con QC:QualC MS:TmList ) => MS ~> #freezeCon(QC, .ValList)
  rule .TmList ~> #freezeCon(QC:QualC, VL:ValList) => ( con QC #reverse(VL) )
  rule (M:Tm MS:TmList => M ~> MS) ~> #freezeCon(QC:QualC, VL:ValList)
  rule V:Val ~> MS:TmList ~> #freezeCon(QC:QualC, VL:ValList)
    => MS ~> #freezeCon(QC, V VL)

  syntax ValList ::= #reverse(ValList)                               [function]
                   | #reverseAux(ValList, ValList)                   [function]

  rule #reverse(V:ValList) => #reverseAux(V, .ValList)
  rule #reverseAux(.ValList, VL:ValList) => VL
  rule #reverseAux(V:Val VL1:ValList, VL2:ValList) => #reverseAux(VL1, V VL2)

  rule (success M):Tm => M ~> #freezeSucc()
  rule V:Val ~> #freezeSucc() => (success V):Val

  rule
    <k> symInt(X:Var) => ?V:Int ... </k>
    <sym> ... .Map => X |-> ?V ... </sym>

endmodule
