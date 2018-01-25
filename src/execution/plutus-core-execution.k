require "plutus-core-syntax.k"
require "hex-conversion.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports KRYPTO
  imports HEX-CONVERSION

  configuration
    <T>
      <k> $PGM:Prg </k>
      <lenv> .Map </lenv>
      <genv> .Map </genv>
      <modules>
        <module multiplicity="*">
          <curr> 0 </curr>
          <name> .ModName </name>
          <import> .ModNmList </import>
          <exportTy> .TyExpList </exportTy>
          <exportTm> .NameList </exportTm>
        </module>
      </modules>
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInteger:Name       I1:Int I2:Int ) => I1 +Int I2
  rule ( builtin subtractInteger:Name  I1:Int I2:Int ) => I1 -Int I2
  rule ( builtin multiplyInteger:Name  I1:Int I2:Int ) => I1 *Int I2
  rule ( builtin divideInteger:Name    I1:Int I2:Int ) => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInteger:Name I1:Int I2:Int ) => I1 %Int I2
    when I2 =/=Int 0

  // TODO: define semantics for compbuiltin.

  syntax Tm ::= #conHolder(K, K)
              | #qualCHolder(K, K)
  rule #conHolder(MN, C) => ( con (MN . C):QualC .ValList )

  rule #stringToModName(S:String)
    => #parseToken("ModName@PLUTUS-CORE-SYNTAX", S)
  rule #stringToCon(S:String) => #parseToken("Con@PLUTUS-CORE-SYNTAX", S)

  syntax Prim ::= "#primTrue" | "#primFalse"
  rule #primTrue
    => #conHolder(#stringToModName("Prelude"), #stringToCon("True"))
  rule #primFalse
    => #conHolder(#stringToModName("Prelude"), #stringToCon("False"))

  rule ( builtin equalsInteger:Name I:Int I:Int        ) => #primTrue
    [transition]
  rule ( builtin equalsInteger:Name I1:Int I2:Int      ) => #primFalse
    when I1 =/=Int I2
    [transition]
  rule ( builtin lessThanInteger:Name I1:Int I2:Int    ) => #primTrue
    when I1 <Int I2
  rule ( builtin lessThanInteger:Name I1:Int I2:Int    ) => #primFalse
    when notBool (I1 <Int I2)
  rule ( builtin greaterThanInteger:Name I1:Int I2:Int ) => #primTrue
    when I1 >Int I2
  rule ( builtin greaterThanInteger:Name I1:Int I2:Int ) => #primFalse
    when notBool (I1 >Int I2)

  rule ( builtin integerToByteString:Name 0 )    => #toByStr("0")
  rule ( builtin integerToByteString:Name I:Int) => #toByStr(#toHex(I))
    when I =/=Int 0

  syntax ByStr ::= #toByStr ( String )                                [function]
  rule #toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule #toString(B:ByStr):String
    => substrString(#toStringVerbatim(B), 1, lengthString(#toStringVerbatim(B)))

  rule ( builtin takeByteString:Name N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), 0, N))

  rule ( builtin dropByteString:Name N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), N, lengthString(#toString(B))))

  rule ( builtin concatenate:Name B1:ByStr B2:ByStr )
    => #toByStr(#toString(B1) +String #toString(B2))

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

  rule
    <k> X:Var => V ... </k>
    <lenv> ... X |-> V ... </lenv>

  // Desugar let expression to lambda expression
  rule ( let N:Tm X:Var M:Tm ):Tm => [ ( lam X M ) N ]

  // Process modules in order
  rule (program M:Mod ML:ModList) => M ~> (program ML:ModList)
  rule (program .ModList) => .K

  // Process the definitions in order.
  rule D:Dec DS:DecList => D ~> DS

  // Ignoring typing stuff
  rule ( declare N:Name T:Ty ) => .K
  rule ( data C:TyCon ( KSL:KSigList ) AL:AltList ) => .K
  rule ( isa M:Tm T:Ty ) => .K
  rule ( abs V:Var M:Tm ):Tm => M
  rule ( inst M:Tm T:Ty ) => .K

  // Begin processing a module, add new <module> cell
  rule
    <k>
      ( module MN:ModName (import MNL) (export (TXL) (NL)) DEFS ) => DEFS ...
    </k>
    (.Bag => <module> ...
               <curr> 1 </curr>
               <name> MN </name>
               <import> MNL </import>
               <exportTy> TXL </exportTy>
               <exportTm> NL </exportTm>
        ... </module>)

  // Add V to env of current module
  rule
    <k> ( define N:Name V:Val ) => .K ... </k>
    <curr> 1 </curr>
    <name> MN </name>
    <lenv> _ => .Map </lenv>
    <genv> ... .Map => (MN . N):QualN |-> V ... </genv>

  // Finishing processing a module as there are no more definitions
  rule <k> .DecList => .K ... </k>
       <curr> 1 => 0 </curr>

  // Qualified Name reduces if it is in the current module
  rule
    <k> (MN . N):QualN => V ... </k>
    <genv> ... (MN . N):QualN |-> V ... </genv>
    <curr> 1 </curr>
    <name> MN </name>

  // Qualified name reduces if the name is exported from a different module
  // which is imported by the current module
  rule
    <k> (MN' . N):QualN => V ... </k>
    <genv> ... (MN' . N):QualN |-> V ... </genv>
    <module>
      <curr> 1 </curr>
      <name> MN </name>
      <import> MNL </import>
      <exportTy> _ </exportTy>
      <exportTm> _ </exportTm>
    </module>
    <module>
      <curr> 0 </curr>
      <name> MN' </name>
      <import> _ </import>
      <exportTy> _ </exportTy>
      <exportTm> NL </exportTm>
    </module>
    when (MN' #in_mn MNL) andBool (N #in_n NL)

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax TmList ::= #toTermList(ValList)                             [function]
  rule #toTermList(.ValList) => .TmList
  rule #toTermList(M:Val MS:ValList) => M #toTermList(MS)

  syntax Tm ::= "#ifTm" Bool "#then" Tm "#else" Tm "#fi"             [function]

  // QualC is ok if it was defined in the current module
  rule
    <k> ( case ( con (MN . C):QualC MS:ValList )
               ( (MN' . C'):QualC ( XS:VarList ) M:Tm ) CL:ClList )
      => #ifTm (C ==K C' andBool MN ==K MN') #then [ ( lam XS M ) #toTermList(MS) ]
                          #else ( case ( con (MN . C):QualC MS:ValList ) CL )
         #fi ...
    </k>
    <curr> 1 </curr>
    <name> MN </name>

  // Or if it was defined in another module and approapriately imported/exported
  // TODO: probably need to check exportTy better
  rule
    <k> ( case ( con (MN' . C):QualC MS:ValList )
               ( (MN'' . C'):QualC ( XS:VarList ) M:Tm ) CL:ClList )
      => #ifTm (C ==K C' andBool MN' ==K MN'')
           #then [ ( lam XS M ) #toTermList(MS) ]
           #else ( case ( con (MN' . C):QualC MS:ValList ) CL )
         #fi ...
    </k>
    <module>
      <curr> 1 </curr>
      <name> MN </name>
      <import> MNL </import>
      <exportTy> _ </exportTy>
      <exportTm> _ </exportTm>
    </module>
    <module>
      <curr> 0 </curr>
      <name> MN' </name>
      <import> _ </import>
      <exportTy> TXL </exportTy>
      <exportTm> _ </exportTm>
    </module>
    when (MN' #in_mn MNL) andBool (C #in_tx TXL)
    // TODO: do we also require C' #in_tx TXL?

  // For using datatypes define in the prelude.
  // TODO: This rule should be removed when we have imports/exports between files
  rule ( case ( con (MN . C):QualC MS:ValList )
              ( (MN' . C'):QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => #ifTm (C ==K C' andBool MN ==K MN')
         #then [ ( lam XS M ) #toTermList(MS) ]
         #else ( case ( con (MN . C):QualC MS:ValList ) CL )
       #fi
    when MN ==K #stringToModName("Prelude")


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

  /***************************************************************************/
  /***********************   AUXILIARY FUNCTIONS  ****************************/
  /***************************************************************************/

  syntax ValList ::= #reverse(ValList)                               [function]
                   | #reverseAux(ValList, ValList)                   [function]

  rule #reverse(V:ValList) => #reverseAux(V, .ValList)
  rule #reverseAux(.ValList, VL:ValList) => VL
  rule #reverseAux(V:Val VL1:ValList, VL2:ValList) => #reverseAux(VL1, V VL2)

  rule (success M):Tm => M ~> #freezeSucc()
  rule V:Val ~> #freezeSucc() => (success V):Val

  syntax Bool ::= ModName "#in_mn" ModNmList                            [function]
                | Name "#in_n" NameList                                [function]

  rule MN:ModName #in_mn .ModNmList => false:Bool
  rule MN:ModName #in_mn MN:ModName MNL:ModNmList => true:Bool
  rule MN:ModName #in_mn MN':ModName MNL:ModNmList => MN #in_mn MNL
    when MN =/=K MN'

  rule N:Name #in_n .NameList => false:Bool
  rule N:Name #in_n N:Name NL:NameList => true:Bool
  rule N:Name #in_n N':Name NL:NameList => N #in_n NL
    when N =/=K N'

endmodule