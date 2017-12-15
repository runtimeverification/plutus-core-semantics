require "plutus-core-syntax.k"
require "substitution.k"

module PLUTUS-CORE-TYPING
  imports PLUTUS-CORE-SYNTAX
  imports SUBSTITUTION

  /*****************************************************************************/
  /**                 SYNTAX FOR SYNTHESIS, CHECKING, ETC.                     */
  /*****************************************************************************/

  syntax K ::= Ty "<-" Tm                                    // `Tm` checks `Ty`
             | TyList "<-s" TmList

             | Ty "|-" Ty "<-" Cl                          // `Cl` has type `Ty`
             | Ty "|-" Ty "<-s" ClList

             | Ty "::" Ki                                  // `Ty` has kind `Ki`
             | TyList "::s" KiList

             | AltList "::a" Ki                        //`AltList` has kind `Ki`

             | "(" Con Map TmList ")"  "in" AltList
             | "(" Con Map VarList ")" "in" AltList

  syntax K ::= #restoreTEnv(Map)
             | #restoreKEnv(Map)

  /*****************************************************************************/
  /**                             CONFIGURATION                                */
  /*****************************************************************************/

  configuration
    <T>
      <k> $PGM:Mod </k>
      <tenv> .Map </tenv>
      <kenv> .Map </kenv>
      <data> .Map </data>
      <mod> .ModName </mod>
    </T>

  // Process the definitions in order.
  rule D:Dec DS:DecList => D ~> DS

  // Add declaration to type environment
  rule
    <k> (declare N:Name TAU:Ty) => .K ... </k>
    <tenv> ... .Map => (N |-> TAU) ... </tenv>

  // Valid data declaration
  rule
    <k> (data C (KSL:KSigList) AL) => AL ::a (type) ~> #restoreKEnv(Rho) ... </k>
    <data> ... .Map => (C |-> (data C (KSL) AL)) ... </data>
    <kenv> Rho => Rho (#getTyVars(KSL) := #getKinds(KSL)) </kenv>

  // AltList has kind K
  rule .AltList ::a K => .K
  rule (C .TyList) AL ::a K => AL ::a K
  rule (C T TL) AL ::a K => T :: K ~> (C TL) AL ::a K

  // Type reduction
  rule [ (lam X:TyVar K T) U:Ty ] => T [ U / X ]

  /*****************************************************************************/
  /**                           HAS KIND JUDGMENT                              */
  /*****************************************************************************/

  // tyvar
  rule
    <k> X :: K => .K ... </k>
    <kenv> ... X |-> K ... </kenv>

  // TODO: rule for tyname?

  // fun
  rule (fun S T):Ty :: (type):Ki => S :: (type):Ki ~> T :: (type):Ki

  // tycon
  rule
    <k>
      (con (MN . TC:TyCon) TL:TyList) :: (type):Ki
        => TL ::s #getKinds(KSL)
        ...
    </k>
    <data> ... TC |-> (data TC (KSL) AL) ... </data>

  // comp
  rule (comp T):Ty :: (type):Ki => T :: (type):Ki

  // forall
  rule
    <k> (forall X:TyVar K:Ki T) :: (type):Ki => T :: (type):Ki ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> K) </kenv>

  // integer
  rule (integer) :: (type):Ki => .K

  // bytestring
  rule (bytestring) :: (type):Ki => .K

  // tylam
  rule
    <k> (lam X J T) :: (fun J K) => T :: K ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> J) </kenv>

  // tyapp
  rule [ (lam X J S) T:Ty ] :: K => (lam X J S) :: (fun J K) ~> T :: J

  rule
    <k> [ X:TyVar T:Ty ] :: K => [ (lam X J S) T ] :: K ... </k>
    <kenv> ... X |-> (lam X J S) ... </kenv>

  rule
    <k> (define N:Name M:Tm) => TAU <- M ... </k>
    <tenv> ... N |-> TAU ... </tenv>

  // abs

  /*****************************************************************************/
  /**                          THE CHECKS JUDGMENT                             */
  /*****************************************************************************/

  // Checks: abs.
  rule
    <k> (forall X:TyVar K:Ki T) <- (abs X M) => T <- M ~> #restoreKEnv(Rho) ... </k>
    <kenv> Rho => Rho (X |-> K) </kenv>

  // Checks: lam.
  rule
    <k> (fun S T) <- (lam X:Var M) => T <- M ~> #restoreTEnv(Rho) ... </k>
    <tenv> Rho => Rho (X |-> S) </tenv>

  // Checks: con.
  rule
    <k>
      (con (MN . TC:TyCon) TYS) <- (con (MN . C:Con) MS:TmList)
        => (C (#getTyVars(KSL) := TYS) MS) in AL
        ...
    </k>
    <data> ... TC |-> (data TC (KSL:KSigList) AL:AltList) ... </data>

  // Helper for checks: con, checking if Con in AltList.
  // Substitution must be done in TyList before checks judgment

  rule (C:Con M:Map MS) in (C:Con TL':TyList) _ => TL' [[ M ]] <-s MS
  rule (C:Con M:Map MS:TmList) in (C':Con TL':TyList) AL:AltList
    => (C M MS) in AL
    requires C =/=K C'

  // Checks: case.
  rule T <- (case (con QC TL) CLS) => (con QC TL) |- T <-s CLS

  // clause - called by checks: case
  rule
    <k>
      (con (MN . TC) TYS) |- T:Ty <- (MN . C:Con (XS) M):Cl
        => (C (#getTyVars(KSL) := TYS) XS) in AL
        ~> T <- M ~> #restoreTEnv(Rho)
        ...
    </k>
    <tenv> Rho </tenv>
    <data> ... TC |-> (data TC (KSL:KSigList) AL:AltList) ... </data>

  // Helper for clause: checking if Con in AltList
  // Substitution must be done in type environment

  rule
    <k> (C:Con M:Map XL:VarList) in (C:Con TL':TyList) _ => .K ... </k>
    <tenv> Rho => Rho (XL := TL' [[ M ]]) </tenv>
  rule (C:Con M:Map XL:VarList) in (C':Con TL':TyList) AL:AltList
    => (C M XL) in AL
    requires C =/=K C'

  // Checks: success.
  rule (comp T) <- (success M) => M <- T

  // Checks: failure.
  rule (comp T) <- (failure) => .K

  // Checks: dirchange
  rule T:Ty <- M:SynthTm => M ~> T

  rule T:Ty ~> T => .K

  /*****************************************************************************/
  /**                       THE SYNTHESIS JUDGMENT                             */
  /*****************************************************************************/

  // Synthesis: var.
  rule
    <k> X:Var => T ... </k>
    <tenv> ... X |-> T ... </tenv>

  // Synthesis: name.
  rule
    <k> (MN . N):QualN => T ... </k>
    <tenv> ... N |-> T ... </tenv>

  // Synthesis: isa
  rule (isa M T):Tm => T <- M ~> T

  // Synthesis: inst
  rule (inst (forall X:TyVar K:Ki T) S):Tm => S :: K ~> T[S / X]

  // Synthesis: app.
  rule [ (fun S T) N:Tm ] => S <- N ~> T

  // compbuiltin
  // TODO.

  // bind
  // TODO.

  // Synthesis: intval.
  rule I:Int => (integer)

  // Synthesis: bytestringval.
  rule B:ByStr => (bytestring)

  // builtins
  rule (builtin addInteger (integer) (integer)):Tm => (integer)
  rule (builtin multiplyInteger (integer) (integer)):Tm => (integer)
  rule (builtin subtractInteger (integer) (integer)):Tm => (integer)
  rule (builtin divideInteger (integer) (integer)):Tm => (integer)
  rule (builtin remainderInteger (integer) (integer)):Tm => (integer)

  rule (builtin lessThanInteger (integer) (integer)):Tm => boolean
  rule (builtin lessThanEqualsInteger (integer) (integer)):Tm => boolean
  rule (builtin greaterThanInteger (integer) (integer)):Tm => boolean
  rule (builtin greaterThanEqualsInteger (integer) (integer)):Tm => boolean
  rule (builtin equalsInteger (integer) (integer)):Tm => boolean

  rule (builtin integerToByteString (integer)):Tm => (bytestring)

  rule (builtin concatenate (bytestring) (bytestring)):Tm => (bytestring)

  rule (builtin takeByteString (integer) (bytestring)):Tm => (bytestring)
  rule (builtin dropByteString (integer) (bytestring)):Tm => (bytestring)

  rule (builtin sha2_256 (bytestring)):Tm => (bytestring)
  rule (builtin sha3_256 (bytestring)):Tm => (bytestring)

  rule (builtin equalsByteString (bytestring) (bytestring)):Tm => boolean

  // Restore type and kind environments
  rule
    <k> #restoreTEnv(Rho) => .K ... </k>
    <tenv> _ => Rho </tenv>

  rule
    <k> #restoreKEnv(Rho) => .K ... </k>
    <kenv> _ => Rho </kenv>

  syntax ModF ::= "(" "moduleF" ModName ImpD ExpD LocD ")"

  rule
    <k>
      (module MN:ModName IS ES LS DEFS) => DEFS ~> (moduleF MN IS ES LS)
    </k>
    <mod> CurrMN => MN </mod>

  rule (.DecList ~> _:ModF) => .K

  /*****************************************************************************/
  /**                        HELPER FUNCTIONALITY                              */
  /*****************************************************************************/

  // Desugar multi-argument application/lambdas

  rule [ F:Val M1 M2 MS ] => [ [ F M1 ] M2 MS ]
  rule T <- (lam X1 X2 XS M) => T <- (lam X1 .VarList (lam X2 XS M))

  // Sugar for boolean type - boolean = Prelude.Bool.
  // #tyconHolder needed for parsing

  syntax Ty ::= #tyconHolder(K, K)
  rule #tyconHolder(MN, TC) => ( con (MN . TC) .TyList )

  rule boolean =>
    #tyconHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"),
                 #parseToken("TyCon@PLUTUS-CORE-SYNTAX", "Bool"))

  // When checking type of case statement, move to the top of the K cell so
  // the first argument evaluates to a value. Then typecheck.

  rule T <- (case M:NonValTm CLS) => (case M CLS) ~> T
  rule (case V:Val CLS) ~> T => T <- (case V CLS)

  // Get Kinds and TyVars from KSigList

  syntax KiList ::= #getKinds(KSigList)                              [function]
  syntax TyVarList ::= #getTyVars(KSigList)                          [function]

  rule #getKinds(.KSigList) => .KiList
  rule #getKinds((X K) KSL) => K #getKinds(KSL)

  rule #getTyVars(.KSigList) => .TyVarList
  rule #getTyVars((X K) KSL) => X #getTyVars(KSL)

  // Bind list of variables to list of types/kinds, pointwise

  syntax Map ::= VarList ":=" TyList                                 [function]
  syntax Map ::= TyVarList ":=" TyList                               [function]
  syntax Map ::= TyVarList ":=" KiList                               [function]

  rule .VarList := .TyList => .Map
  rule V:Var VL := T TL => V |-> T (VL := TL)

  rule .TyVarList := .TyList => .Map
  rule TV:TyVar TVL := T:Ty TL => TV |-> T (TVL := TL)

  rule .TyVarList := .KiList => .Map
  rule TV:TyVar TVL := K:Ki KL => TV |-> K (TVL := KL)

  // Helper for substitution, needed for parsing

  syntax Ty ::= #subst(Ty, Map)                                      [function]
  rule #subst(T, M) => T [ M ]

  // Perform substitution in a TyList pointwise via a Map

  syntax TyList ::= TyList "[[" Map "]]"                             [function]

  rule .TyList [[ _:Map ]] => .TyList
  rule (T TL):TyList [[ M:Map ]] => #subst(T, M) (TL [[ M ]])

  // Types check Terms

  rule .TyList <-s .TmList => .K
  rule T TL <-s M ML => T <- M ~> TL <-s ML

  // Type checks list of clauses

  rule _ |- _ <-s .ClList => .K
  rule T1 |- T2 <-s CL CLS => T1 |- T2 <- CL ~> T1 |- T2 <-s CLS

  // Types have kinds KS

  rule .TyList ::s .KiList => .K
  rule T TL ::s K KL => T :: K ~> TL ::s KL

endmodule
