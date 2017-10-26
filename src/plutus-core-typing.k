require "plutus-core-syntax.k"
require "hex-conversion.k"

module PLUTUS-CORE-TYPING
  imports PLUTUS-CORE-SYNTAX
  imports KRYPTO
  imports HEX-CONVERSION

  syntax Bool ::= checks(Ty, Tm)
                | synth(Tm, Ty)
                // TODO: bug in spec with type application for checksCon?
                | checksCon(QualC, Ty)
                | hasKind(Ty, Ki)

  syntax K ::= checksList(TyList, TmList)

  syntax K ::= #restoreTEnv(Map)

  syntax KiList ::= #getKinds(KSigList)

  // TODO: add syntax (and semantics) for #getKinds, #getVars, etc.

  configuration
    <T>
      <k> $PGM:Mod </k>
      <tenv> .Map </tenv>
      <kenv> .Map </kenv>
      <data> .Map </data>
      <mod> .ModName </mod>
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInt:Name       I1:Int I2:Int ) => I1 +Int I2
  rule ( builtin subtractInt:Name  I1:Int I2:Int ) => I1 -Int I2
  rule ( builtin multiplyInt:Name  I1:Int I2:Int )
    => I1 *Int I2
  rule ( builtin divideInt:Name    I1:Int I2:Int )
    => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInt:Name I1:Int I2:Int )
    => I1 %Int I2
    when I2 =/=Int 0

  // TODO: define semantics for compbuiltin.

  syntax Tm ::= #conHolder(K, K)
              | #qualCHolder(K, K)
  rule #conHolder(MN, C) => ( con (MN . C) .ValList )

  rule #stringToModName(S:String)
    => #parseToken("ModName@PLUTUS-CORE-SYNTAX", S)
  rule #stringToCon(S:String) => #parseToken("Con@PLUTUS-CORE-SYNTAX", S)

  syntax Prim ::= "#primTrue" | "#primFalse"
  rule #primTrue
    => #conHolder(#stringToModName("Prelude"), #stringToCon("True"))
  rule #primFalse
    => #conHolder(#stringToModName("Prelude"), #stringToCon("False"))

  rule ( builtin equalsInt:Name I:Int I:Int     ) => #primTrue
                                                       [transition]
  rule ( builtin equalsInt:Name I1:Int I2:Int   ) => #primFalse
                                                       when I1 =/=Int I2
                                                       [transition]
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #primTrue
                                                       when I1 <Int I2
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #primFalse
                                                       when notBool (I1 <Int I2)

  rule ( builtin intToByteString:Name 0 )    => #toByStr("0")
  rule ( builtin intToByteString:Name I:Int)
    => #toByStr(#toHex(I))
    when I =/=Int 0

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

  // TODO: no gas encoding chosen for now
  rule ( builtin sha3_256:Name B:ByStr ) => #toByStr(Sha3256(#toString(B)))
  rule ( builtin sha2_256:Name B:ByStr ) => #toByStr(Sha256(#toString(B)))

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr )
    => #primTrue when #toString(B1) ==String #toString(B2)

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr )
    => #primFalse when #toString(B1) =/=String #toString(B2)

  /***************************************************************************/
  /************************   LOCAL REDUCTIONS   *****************************/
  /***************************************************************************/

  rule [ F:Val M1:Tm M2:Tm MS:TmList ] => [ [ F M1 ] M2 MS ]

  // From desugaring constructors, we may have lambda expressions with no
  // variables and application to an empty TmList.
  rule ( lam .VarList M:Tm ) => M
  rule [ V:Val .TmList ] => V

  // When a lambda abstracts more than one variable, desugar it to a
  // combination of unary lambdas.
  rule ( lam X1:Var X2:Var XS:VarList M:Tm )
    => ( lam X1 .VarList ( lam X2 XS M ) )

  syntax Val ::= closure(Map, Var, Tm)

  // Wrapper around function application in order to make application strict
  syntax Tm ::= app(Tm, Tm)                                            [strict]
  rule [ F:Val M:Tm ] => app(F, M)

  // Desugar let expression to lambda expression
  rule ( let N:Tm X:Var M:Tm ):Tm => [ ( lam X M ) N ]

  // Process the definitions in order.
  rule D:Dec DS:DecList => D ~> DS

  rule
    <k> ( declare N:Name TAU:Ty ) => .K ... </k>
    <tenv> ... .Map => (N |-> TAU) ... </tenv>

  rule
    <k> hasKind(X, K) => true ... </k>
    <kenv> ... X |-> K ... </kenv>

  rule hasKind(( fun S T ), (type)) => hasKind(S, (type)) ~> hasKind(T, (type))

  rule
    <k> hasKind(( con QC TL ), (type)) => hasKindList(TL, #getKinds(KSL)) ... </k>
    <data> ... C |-> ( data C ( KSL ) AL ) ... </data>

  rule hasKind(( comp T ), (type)) => hasKind(T, (type))

  rule
    <k> hasKind(( forall X K T ), (type)) => hasKind(T, (type)) ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> K) </kenv>

  rule hasKind((integer)<:Ty, (type)) => true
  rule hasKind((bytestring)<:Ty, (type)) => true

  rule
    <k> hasKind(( lam X J T ), ( fun J K )) => hasKind(T, K) ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> J) </kenv>

  rule hasKind( [ S T ], K) => hasKind(S, ( fun ?J K )) ~> hasKind(T, ?J)

  rule
    <k> ( define N:Name M:Tm ) => checks(TAU, M) ... </k>
    <tenv> ... N |-> TAU ... </tenv>

  rule checks((integer)<:Ty, I:Int) => synth(I:Int, (integer)<:Ty)

  rule
    <k> checks(T:Ty, X:Var) => true ... </k>
    <tenv> ... X |-> T ... </tenv>

  rule synth(I:Int, (integer)<:Ty) => true

  rule
    <k> checks(( forall X K T ), ( abs X M )) => checks(T, M) ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> K) </kenv>

  rule
    <k> checks(( fun S .TyList T ), ( lam X:Var .VarList M:Tm )) => checks(T, M) ~> #restoreTEnv(Rho) ... </k>
    <tenv> Rho => Rho (X |-> S) </tenv>

  // rule
  //   <k>
  //     checks(( con (MN . C) TYS ), ( con QC MS ))
  //     => checksCon(QC, ( forall #getVars(TYS) #getKinds(KSL)
  //                               ( fun #getVars(TYS) ( con (MN . C) #getVars(TYS) ) ) ))
  //     ~> checksList(TYS, MS)
  //   </k>
  //   <data> ... C |-> ( data C ( KSL ) AL ) ... </data>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS )
    </k>
    <mod> CurrMN => MN </mod>

  rule (.DecList ~> _:ModF ) => .K

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax TmList ::= #toTermList(ValList)                             [function]
  rule #toTermList(.ValList) => .TmList
  rule #toTermList(M:Val MS:ValList) => M #toTermList(MS)

  syntax Tm ::= "#ifTm" Bool "#then" Tm "#else" Tm "#fi"             [function]

  rule ( case ( con QC:QualC MS:ValList )
              ( QC':QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => #ifTm (QC ==K QC') #then [ ( lam XS M ) #toTermList(MS) ]
                          #else ( case ( con QC:QualC MS:ValList ) CL )
       #fi

  rule #ifTm true #then M #else _ #fi => M
  rule #ifTm false #then _ #else M #fi => M

  // Convert TmList into ValList and convert (success Tm) into (success Val)
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
endmodule
