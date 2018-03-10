require "../syntax.k"
require "hex-conversion.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports HEX-CONVERSION
// --- TODO: Disabled KRYPO support to ease transition to K5
//  imports KRYPTO

// K5 doesn't know how to parse <module multiplicity='*' type='...'> ?
  syntax #CellName ::= "type" [token]

  configuration
    <T>
      <k> $PGM:Prg </k>
      <lenv> .Map </lenv>
      <genv> .Map </genv>
      <modules>
        // K5 Does not like cells named 'module'
        <Module multiplicity="*" type="Set">
          <curr> 0 </curr>
          <name> .ModName </name>
          <dataCons> .TyExpList </dataCons>
          <decl> .NameList:NameList </decl>
          <importedModules> .ModNmList </importedModules>
          <exportTy> .TyExpList </exportTy>
          <exportTm> .NameList:NameList </exportTm>
        </Module>
      </modules>
    </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInteger       I1:Int I2:Int ) => I1 +Int I2
  rule ( builtin subtractInteger  I1:Int I2:Int ) => I1 -Int I2
  rule ( builtin multiplyInteger  I1:Int I2:Int ) => I1 *Int I2
  rule ( builtin divideInteger    I1:Int I2:Int ) => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInteger I1:Int I2:Int ) => I1 %Int I2
    when I2 =/=Int 0

  // TODO: define semantics for compbuiltin.

  syntax Tm ::= #conHolder(K, K)
              | #qualCHolder(K, K)
  rule #conHolder(MN, C) => ( con (MN . C):QualC .ValList )

  rule #stringToModName(S:String)
    => #parseToken("ModName", S)
  rule #stringToCon(S:String) => #parseToken("Con", S)

  syntax Prim ::= "#primTrue" | "#primFalse"
  rule #primTrue
    => #conHolder(#stringToModName("Prelude"), #stringToCon("True"))
  rule #primFalse
    => #conHolder(#stringToModName("Prelude"), #stringToCon("False"))

  rule ( builtin equalsInteger I:Int I:Int        ) => #primTrue
    [transition]
  rule ( builtin equalsInteger I1:Int I2:Int      ) => #primFalse
    when I1 =/=Int I2
    [transition]
  rule ( builtin lessThanInteger I1:Int I2:Int    ) => #primTrue
    when I1 <Int I2
  rule ( builtin lessThanInteger I1:Int I2:Int    ) => #primFalse
    when notBool (I1 <Int I2)
  rule ( builtin greaterThanInteger I1:Int I2:Int ) => #primTrue
    when I1 >Int I2
  rule ( builtin greaterThanInteger I1:Int I2:Int ) => #primFalse
    when notBool (I1 >Int I2)

  rule ( builtin integerToByteString 0 )    => #toByStr("0")
  rule ( builtin integerToByteString I:Int) => #toByStr(#toHex(I))
    when I =/=Int 0

  syntax ByStr ::= #toByStr ( String )                                [function]
  rule #toByStr(S:String)
    => #parseToken("ByStr@PLUTUS-CORE-SYNTAX", ("#" +String S))

  rule #toString(B:ByStr):String
    => substrString(#toStringVerbatim(B), 1, lengthString(#toStringVerbatim(B)))

  rule ( builtin takeByteString N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), 0, N))

  rule ( builtin dropByteString N:Int B:ByStr )
    => #toByStr(substrString(#toString(B), N, lengthString(#toString(B))))

  rule ( builtin concatenate B1:ByStr B2:ByStr )
    => #toByStr(#toString(B1) +String #toString(B2))

// --- TODO: Disabled KRYPO support to ease transition to K5
//  rule ( builtin sha3_256 B:ByStr ) => #toByStr(Sha3256(#toString(B)))
//  rule ( builtin sha2_256 B:ByStr ) => #toByStr(Sha256(#toString(B)))

  rule ( builtin equalsByteString B1:ByStr B2:ByStr )
    => #primTrue when #toString(B1) ==String #toString(B2)

  rule ( builtin equalsByteString B1:ByStr B2:ByStr )
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
  rule ( isa M:Tm T:Ty ) => .K
  rule ( abs V:Var M:Tm ):Tm => M
  rule ( inst M:Tm T:Ty ) => .K

  // Begin processing a module, add new <Module> cell
  //rule ( module MN:ModName (import MNL) (export (TXL) (NL)) DEFS ) .DecList => DEFS

  rule
    <k>
      ( module MN:ModName (import MNL) (export (TXL) (NL:NameList)) DEFS ) => DEFS ...
    </k>
    (.Bag => <Module> ...
               <curr> 1 </curr>
               <name> MN </name>
               <dataCons> .TyExpList </dataCons>
               <decl> .NameList:NameList </decl>
               <importedModules> MNL </importedModules>
               <exportTy> TXL </exportTy>
               <exportTm> NL </exportTm>
        ... </Module>)

  // Add data declaration to dataCons subcell
  rule
    <k> ( data TC:TyCon ( KSL:KSigList ) AL:AltList ) => .K ... </k>
    <curr> 1 </curr>
    <dataCons> DL => ( TC ( #getConListAlt(AL) ) ) DL </dataCons>

  // Add declared name to decls
  rule
    <k> ( declare N T:Ty ) => .K ... </k>
    <curr> 1 </curr>
    <decl> DL => N DL </decl>

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
    <decl> DL:NameList </decl>
    when N #in_n DL

  // Qualified name reduces if the name is exported from a different module
  // which is imported by the current module
  rule
    <k> (MN' . N):QualN => V ... </k>
    <genv> ... (MN' . N):QualN |-> V ... </genv>
    <Module>
      <curr> 1 </curr>
      <name> MN </name>
      <dataCons> _ </dataCons>
      <decl> _ </decl>
      <importedModules> MNL </importedModules>
      <exportTy> _ </exportTy>
      <exportTm> _ </exportTm>
    </Module>
    <Module>
      <curr> 0 </curr>
      <name> MN' </name>
      <dataCons> _ </dataCons>
      <decl> DL:NameList </decl>
      <importedModules> _ </importedModules>
      <exportTy> _ </exportTy>
      <exportTm> NL:NameList </exportTm>
    </Module>
    when (MN' #in_mn MNL) andBool (N #in_n NL) andBool (N #in_n DL)

  /***************************************************************************/
  /************************   PATTERN MATCHING   *****************************/
  /***************************************************************************/

  syntax TmList ::= #toTermList(ValList)                             [function]
  rule #toTermList(.ValList) => .TmList
  rule #toTermList(M:Val MS:ValList) => M #toTermList(MS)

  syntax Tm ::= "#ifTm" Bool "#then" Tm "#else" Tm "#fi"             [function]
  rule #ifTm true #then M #else _ #fi => M
  rule #ifTm false #then _ #else M #fi => M

  // Marker that case match is valid to be executed
  syntax K ::= "#valid"

  // Case match is valid if the type was defined in the current module and
  // all clauses match all constructors
  rule
    <k> ( case M CLS ) => #valid ~> ( case M CLS ) ... </k>
    <curr> 1 </curr>
    <name> MN </name>
    <dataCons> TXL </dataCons>
    when #getConListCl(CLS) #in_tx TXL

  // Case match is valid if the type was defined in another module, all
  // clauses match all constructors, all constructors are exported from that
  // module, and that module is importad from the current module
  rule
    <k> ( case M CLS ) => #valid ~> ( case M CLS ) ... </k>
    <Module>
      <curr> 1 </curr>
      <name> MN </name>
      <dataCons> _ </dataCons>
      <decl> _ </decl>
      <importedModules> MNL </importedModules>
      <exportTy> _ </exportTy>
      <exportTm> _ </exportTm>
    </Module>
    <Module>
      <curr> 0 </curr>
      <name> MN' </name>
      <dataCons> TXL </dataCons>
      <decl> _ </decl>
      <importedModules> _ </importedModules>
      <exportTy> TXL' </exportTy>
      <exportTm> _ </exportTm>      
    </Module>
    when #getConListCl(CLS) #in_tx TXL andBool #getConListCl(CLS) #in_tx TXL'
                                       andBool MN' #in_mn MNL

  // Can progress processing case match if it was determined valid
  rule
    #valid ~>
      ( ( case ( con (MN . C):QualC MS:ValList )
               ( (MN . C'):QualC ( XS:VarList ) M:Tm ) CL:ClList )
        => #ifTm (C ==K C')
             #then [ ( lam XS M ) #toTermList(MS) ]
             #else ( case ( con (MN . C):QualC MS:ValList ) CL )
           #fi
      )

  // clear #valid off K cell if we are no longer processing case match
  rule (#valid => .K) ~> [ M:Tm MS ]

  // For using datatypes define in the prelude.
  // TODO: This rule should be removed when we have imports/exports between files
  rule (.K => #valid) ~> ( case ( con (MN . C):QualC MS:ValList ) CLS:ClList )
    when MN ==K #stringToModName("Prelude")

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

  syntax Bool ::= ModName "#in_mn" ModNmList                         [function]
                | Name "#in_n" NameList                              [function]
                | ConList "#in_tx" TyExpList                         [function]
                | Con "#in_cl" ConList                               [function]

  // module name in module name list
  rule MN:ModName #in_mn .ModNmList => false:Bool
  rule MN:ModName #in_mn MN:ModName _:ModNmList => true:Bool
  rule MN:ModName #in_mn MN':ModName MNL:ModNmList => MN #in_mn MNL
    when MN =/=K MN'

  // name in name list
  rule N:Name #in_n .NameList => false:Bool
  rule N:Name #in_n N:Name _:NameList => true:Bool
  rule N:Name #in_n N':Name NL:NameList => N #in_n NL
    when N =/=K N'

  // con in con list
  rule C:Con #in_cl .ConList => false:Bool
  rule C:Con #in_cl C:Con _:ConList => true:Bool
  rule C:Con #in_cl C':Con CL:ConList => C #in_cl CL
    when C =/=K C'

  // conlist in tyexplist
  rule CL:ConList #in_tx .TyExpList => false:Bool
  rule CL:ConList #in_tx (_:TyCon (CL')) _:TyExpList => true:Bool
    when CL #equal_cl CL'
  rule CL:ConList #in_tx _:TyName TXL:TyExpList => CL #in_tx TXL
  rule CL:ConList #in_tx (_:TyCon (CL')) TXL:TyExpList => CL #in_tx TXL
    when notBool (CL #equal_cl CL')

  // for equality of two conlists when viewed as sets
  syntax Bool ::= ConList "#equal_cl" ConList                        [function]
                | ConList "#subset_cl" ConList                       [function]

  rule CL #equal_cl CL' => CL #subset_cl CL' andBool CL' #subset_cl CL

  rule .ConList #subset_cl CL' => true:Bool
  rule (C CL) #subset_cl CL' => C #in_cl CL' andBool CL #subset_cl CL'

  // Gathering Cons from AltList, ClList
  syntax ConList ::= #getConListAlt(AltList)                         [function]
                   | #getConListCl(ClList)                           [function]

  // Get ConList from AltList
  rule #getConListAlt(.AltList) => .ConList
  rule #getConListAlt( (C:Con TL:TyList) AL:AltList ) => C #getConListAlt(AL)

  // Get ConList from ClList
  rule #getConListCl(.ClList) => .ConList
  rule #getConListCl( ( (MN . C):QualC ( XS:VarList ) M:Tm ) CL:ClList )
    => C #getConListCl(CL)

endmodule
