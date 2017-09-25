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
      <gas> 10000 </gas>
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  syntax K ::= #gas(K, Int)
             | "ERROR"

  rule
    <k> #gas(K, I) => K ... </k>
    <gas> G => G -Int I </gas>
    when (G -Int I) >=Int 0

  rule
    <k> (#gas(K, I) ~> _) => ERROR </k>
    <gas> G </gas>
    when (G -Int I) <Int 0

  rule ( builtin addInt:Name       I1:Int I2:Int ) => #gas(I1 +Int I2, 1)
  rule ( builtin subtractInt:Name  I1:Int I2:Int ) => #gas(I1 -Int I2, 1)
  rule ( builtin multiplyInt:Name  I1:Int I2:Int )
    => #gas(I1 *Int I2, #sizeTm(I1 +Int I2))
  rule ( builtin divideInt:Name    I1:Int I2:Int )
    => #gas(I1 /Int I2, #sizeTm(I1 +Int I2))
    when I2 =/=Int 0
  rule ( builtin remainderInt:Name I1:Int I2:Int )
    => #gas(I1 %Int I2, #sizeTm(I1 +Int I2))
    when I2 =/=Int 0

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

  rule ( builtin equalsInt:Name I:Int I:Int     ) => #gas(#primTrue, 1)
                                                       [transition]
  rule ( builtin equalsInt:Name I1:Int I2:Int   ) => #gas(#primFalse, 1)
                                                       when I1 =/=Int I2
                                                       [transition]
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #gas(#primTrue, 1)
                                                       when I1 <Int I2
  rule ( builtin lessThanInt:Name I1:Int I2:Int ) => #gas(#primFalse, 1)
                                                       when notBool (I1 <Int I2)

  rule ( builtin intToByteString:Name 0 )    => #gas(#toByStr("0"), 1)
  rule ( builtin intToByteString:Name I:Int)
    => #gas(#toByStr(#toHex(I)), #sizeTm(I))
    when I =/=Int 0

  syntax ByStr ::= #toByStr ( String )                                [function]
  rule #toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule #toString(B:ByStr):String
    => substrString(#toStringVerbatim(B), 1, lengthString(#toStringVerbatim(B)))

  rule ( builtin take:Name N:Int B:ByStr )
    => #gas(#toByStr(substrString(#toString(B), 0, N)), N)

  rule ( builtin drop:Name N:Int B:ByStr )
    => #gas(#toByStr(
         substrString(#toString(B), N, lengthString(#toString(B)))), N)

  rule ( builtin concatenate:Name B1:ByStr B2:ByStr )
    => #gas(#toByStr(
         #toString(B1) +String #toString(B2)),
       lengthString(#toString(B1)) +Int lengthString(#toString(B2)))

  // TODO: no gas encoding chosen for now
  rule ( builtin sha3_256:Name B:ByStr ) => #toByStr(Sha3256(#toString(B)))
  rule ( builtin sha2_256:Name B:ByStr ) => #toByStr(Sha256(#toString(B)))

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr )
    => #gas(#primTrue,
            lengthString(#toString(B1)) +Int lengthString(#toString(B2)))
       when #toString(B1) ==String #toString(B2)

  rule ( builtin equalsByteString:Name B1:ByStr B2:ByStr )
    => #gas(#primFalse,
            lengthString(#toString(B1)) +Int lengthString(#toString(B2)))
       when #toString(B1) =/=String #toString(B2)

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
    <gas> G => G -Int (#sizeTm(V) +Int size(Rho)) </gas>
    when G -Int (#sizeTm(V) +Int size(Rho)) >=Int 0
    [structural]

  rule
    <k> (app(closure(Rho, X, M), V:Val) ~> _) => ERROR </k>
    <gas> G </gas>
    when G -Int (#sizeTm(V) +Int size(Rho)) <Int 0
    [structural]

  // rule
  //   <k> (app(closure(Rho, X, M), V:Val) => M ~> Rho') ~> Rest </k>
  //   <lenv> Rho' => Rho[X <- V] </lenv>
  //   <gas> G => G -Int size(Rest) </gas>
  //   [structural]

  rule
    <k> _:Val ~> (Rho => .K) ... </k>
    <lenv> _ => Rho </lenv>
    [structural]

  rule
    <k> X:Var => V ... </k>
    <lenv> ... X |-> V ... </lenv>
    <gas> G => G -Int #sizeTm(V) </gas>
    when (G -Int #sizeTm(V)) >=Int 0

  rule
    <k> (X:Var ~> _) => ERROR </k>
    <lenv> ... X |-> V ... </lenv>
    <gas> G </gas>
    when (G -Int #sizeTm(V)) <Int 0

  // Desugar let expression to lambda expression
  rule ( let N:Tm X:Var M:Tm ):Tm => [ ( lam X M ) N ]

  // Process the definitions in order.
  rule D:Def DS:DefList => D ~> DS

  rule
    <k> ( define N:Name V:Val ) => .K ... </k>
    <mod> MN </mod>
    <lenv> _ => .Map </lenv>
    <genv> ... .Map => (MN . N):QualN |-> V ... </genv>
    <gas> G => G -Int #sizeTm(V) </gas>
    when (G -Int #sizeTm(V)) >=Int 0

  rule
    <k> (( define N:Name V:Val ) ~> _) => ERROR </k>
    <gas> G </gas>
    when (G -Int #sizeTm(V)) <Int 0

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      ( module MN:ModName IS ES LS DEFS ) => DEFS ~> ( moduleF MN IS ES LS )
    </k>
    <mod> CurrMN => MN </mod>

  rule
    <k> QN:QualN => V ... </k>
    <genv> ... QN |-> V ... </genv>

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

  rule
    <k> symInt(X:Var) => ?V:Int ... </k>
    <sym> ... .Map => X |-> ?V ... </sym>

  syntax Int ::= #sizeTm(Tm)                        [function, smt-lib(sizeTm)]
               | #sizeTmList(TmList)                                 [function]
               | #sizeValList(ValList)                               [function]
               | #sizeCl(Cl)                                         [function]
               | #sizeClList(ClList)                                 [function]
               | #sizeVarList(VarList)                               [function]
               | #sizeInt(Int)                     [function, smt-lib(sizeInt)]

  rule #sizeTmList(.TmList) => 0
  rule #sizeTmList(M MS) => #sizeTm(M) +Int #sizeTmList(MS)
  rule #sizeValList(.ValList) => 0
  rule #sizeValList(V VS) => #sizeTm(V) +Int #sizeValList(VS)

  rule #sizeVarList(.VarList) => 0
  rule #sizeVarList(X XS) => 1 +Int #sizeVarList(XS)

  rule #sizeClList(.ClList) => 0
  rule #sizeClList(CL CLS) => #sizeCl(CL) +Int #sizeClList(CLS)

  rule #sizeCl( ( cl QC ( XS ) M ) ) => 2 +Int #sizeVarList(XS) +Int #sizeTm(M)

  rule #sizeTm(X:Var) => 1
  rule #sizeTm(F:QualN) => 1
  rule #sizeTm( ( let M1 X M2 ) ) => 2 +Int #sizeTm(M1) +Int #sizeTm(M2)
  rule #sizeTm( [ M MS ] ) => 1 +Int #sizeTm(M) +Int #sizeTmList(MS)
  rule #sizeTm( ( case M CLS ) ) => 1 +Int #sizeTm(M) +Int #sizeClList(CLS)
  rule #sizeTm( ( con QC MS ) ) => 2 +Int #sizeTmList(MS)
  rule #sizeTm( ( con QC VS ) ) => 2 +Int #sizeValList(VS)
  rule #sizeTm( ( builtin N M ) ) => 2 +Int #sizeTm(M)
  rule #sizeTm( ( builtin N M1 M2 ) ) => 2 +Int #sizeTm(M1) +Int #sizeTm(M1)
  rule #sizeTm( ( success M ) ) => 1 +Int #sizeTm(M)
  rule #sizeTm( ( bind M1 X M2 ) ) => 2 +Int #sizeTm(M1) +Int #sizeTm(M2)
  rule #sizeTm( ( lam XS:VarList M ) ) => 1 +Int #sizeVarList(XS) +Int #sizeTm(M)

  // Values
  // TODO: finish gas calculation for all values
  rule #sizeTm( closure(L, X, M) ) => size(L) +Int 2 +Int #sizeTm(M)

  // TODO: The Float2Int(logFloat(...)) calculates the size of an integer, but
  // verification doesn't like it. We either need to change encoding of an
  // integer or make verification go through
  rule #sizeTm(I:Int) => #sizeInt(I) // Float2Int(logFloat(Int2Float(I, 53, 11)))

  rule #sizeTm(B:ByStr) => 1
  rule #sizeTm( ( failure ):InstrVal ) => 1
  rule #sizeTm( ( success V:Val ) ) => 1 +Int #sizeTm(V)

  rule #sizeInt(I:Int) => 1 // TODO: fix this

  // Gas TODOs:
  // Agree on gas calculation for every rule (sha, application, etc.)
  // Agree on cost for integer/string/bytes operations and storing them
  // Make verification go through with size of int (if that encoding is chosen)
  // Finish gas calculation for all values
  // Refactoring/cleaneup: less repeated code, etc.
  // get #sizeTm to work symbolically with verification (smt-lib attribute?)

endmodule
