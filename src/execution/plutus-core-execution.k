require "plutus-core-syntax.k"
require "hex-conversion.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports KRYPTO
  imports HEX-CONVERSION

  configuration
    <T>
      initERC20Cell(Init)  // including the ERC20-K configuration.
      // <k> $PGM:Prg </k>
      <lenv> .Map </lenv>
      <genv> .Map </genv>
      <mod> .ModName </mod>
      // <state> .Map </state>
      // <in stream="stdin"> .List </in>
      // <out stream="stdout"> .List </out>
      <throwAsPrint> false:Bool </throwAsPrint> // for print THROW
      <printStrings> true:Bool </printStrings>  // for tests
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
  rule (program M:Mod ML:ModList) => M ~> ML:ModList
  rule .ModList => .K

  // Process the definitions in order.
  rule D:Dec DS:DecList => D ~> DS

  // Ignoring typing stuff
  rule ( declare N:Name T:Ty ) => .K
  rule ( data C:TyCon ( KSL:KSigList ) AL:AltList ) => .K
  rule ( isa M:Tm T:Ty ) => .K
  rule ( abs V:Var M:Tm ):Tm => M
  rule ( inst M:Tm T:Ty ) => .K

  rule
    <k> ( define N:Name V:Val ) => .K ... </k>
    <mod> MN </mod>
    <lenv> _ => .Map </lenv>
    <genv> ... .Map => (MN . N):QualN |-> V ... </genv>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS ) ...
    </k>
    <mod> CurrMN => MN </mod>

  rule
    <k> QN:QualN => V ... </k>
    <genv> ... QN |-> V ... </genv>

  rule (.DecList ~> _:ModF ) => .K
  rule .DecList => .K

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

  /***************************************************************************/
  /************************   ERC 20 CONSTRUCTS   ****************************/
  /***************************************************************************/

  rule _:Bool; => .K
  rule _:Int; => .K

  // ---PREAMBLE----------
  // This preamble creates some initial configuration:
  //   accounts, allowances, supply, caller.
  // Recall that this ERC20 semantics assumes that the total supply, all the
  // accounts and all their allowances (including those which are 0), as well
  // as the caller given.  Then the code is executed on the caller's behalf.

  rule test.preamble() =>

    makeAccount(10,100)
    makeAccount(9,90)
    makeAccount(8,80)
    makeAccount(7,70)
    makeAccount(6,60)
    makeAccount(5,50)
    makeAccount(4,40)
    makeAccount(3,30)
    makeAccount(2,20)
    makeAccount(1,10)

    test.allowances(10)
    test.allowances(9)
    test.allowances(8)
    test.allowances(7)
    test.allowances(6)
    test.allowances(5)
    test.allowances(4)
    test.allowances(3)
    test.allowances(2)
    test.allowances(1)

    makeTotalSupply(550)
    (declare caller integer)
    (define caller 7)
    makeCaller(#stringToModName("Foo") . caller)

  rule test.allowances(I:Int) =>

    makeAllowance(I,10)
    makeAllowance(I,9)
    makeAllowance(I,8)
    makeAllowance(I,7)
    makeAllowance(I,6)
    makeAllowance(I,5)
    makeAllowance(I,4)
    makeAllowance(I,3)
    makeAllowance(I,2)
    makeAllowance(I,1)

  // Next we define the special "make" constructs which create and modify the
  // ERC20-K state.  One should be careful with these constructs, because any
  // mistake can make ERC20-K behave unexpectedly.  Such operations should not
  // exist in normal implementations of languages for smart contracts.

  rule <k> makeAccount(A,V) => .K ...</k>
       <accounts>... .Bag => <account> <id> A </id> <balance> V </balance> </account> ...</accounts>
       <allowances>... .Bag => <allowance> <owner> A </owner> <spenders> .Bag </spenders> </allowance> ...</allowances>
  rule <k> makeAllowance(A,B) => .K ...</k> <owner> A </owner>
       <spenders>... .Bag => <allow> <spender> B </spender> <amount> 0 </amount> </allow> ...</spenders>
  rule <k> makeTotalSupply(I) => .K ...</k> <supply> _ => I </supply> requires I >=Int 0
  rule <k> makeCaller(I:Int) => .K ...</k> <caller> _ => I </caller>
  rule <k> makeBalance(I:Int) => .K ...</k> <caller> Owner </caller> <id> Owner </id> <balance> _ => I </balance>
  rule <k> makeThrowAsPrintTrue  => .K ...</k> <throwAsPrint> _ => true </throwAsPrint>
  rule <k> makeThrowAsPrintFalse => .K ...</k> <throwAsPrint> _ => false </throwAsPrint>
//  rule <k> throw => false ...</k> <throwAsPrint> true </throwAsPrint> <out>... .List => ListItem("THROW\n") </out>
  rule <k> makePrintStringsTrue  => .K ...</k> <printStrings> _ => true  </printStrings>
  rule <k> makePrintStringsFalse => .K ...</k> <printStrings> _ => false </printStrings>


endmodule
