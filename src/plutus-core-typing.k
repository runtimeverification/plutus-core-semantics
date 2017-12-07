require "plutus-core-syntax.k"
require "hex-conversion.k"
require "substitution.k"

module PLUTUS-CORE-TYPING
  imports PLUTUS-CORE-SYNTAX
  imports KRYPTO
  imports HEX-CONVERSION
  imports SUBSTITUTION

  // syntax Bool ::= checks(Ty, Tm)
                // | synth(Tm, Ty)
                // TODO: bug in spec with type application for checksCon?
                // | hasKind(Ty, Ki)

  syntax K ::= checksList(TyList, TmList)
             | Ty "<-" Tm                                    // `Tm` checks `Ty`
             | TyList "<-s" TmList
             | Ty "|-" Ty "<-" Cl
             | Ty "|-" Ty "<-s" ClList
             | Ty "::" Ki                                  // `Ty` has kind `Ki`
             | TyList "::" KiList
             | Con "in" AltList
             | checksCon(QualC, Ty)

  syntax K ::= #restoreTEnv(Map)
             | #restoreKEnv(Map)

  // TODO: add syntax (and semantics) for #getKinds, #getVars, etc.

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

  rule
    <k> (declare N:Name TAU:Ty) => .K ... </k>
    <tenv> ... .Map => (N |-> TAU) ... </tenv>

  rule
    <k> (data C (KSL:KSigList) AL) => .K ... </k>
    <data> ... .Map => (C |-> (data C (KSL) AL)) ... </data>

  // Type reduction
  rule [ (lam X:TyVar K T) U:Ty ] => T[U / X]

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
    <k> (con QC:QualTyC TL:TyList) :: (type):Ki => TL :: #getKinds(KSL) ... </k>
    <data> ... C |-> (data C (KSL) AL) ... </data>

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
  // TODO: make sure this is right
  rule
    <k>
      (con (MN . TC:TyCon) TYS) <- (con (MN . C:Con) MS:TmList)
        => C in AL
        ~> #getTys(AL) [ #getTyVars(AL) := TYS ] <-s MS
        ...
    </k>
    <data> ... TC |-> (data TC (KSL:KSigList) AL:AltList) ... </data>

  // Checks: case.
  rule T <- (case (con QC TL) CLS) => (con QC TL) |- T <-s CLS

  // clause
  rule
    <k>
      (con (MN . TC) TYS) |- T:Ty <- (MN . C (XS) M):Cl
        => C in AL ~> T <- M ~> #restoreTEnv(Rho)
        ...
    </k>
    <tenv> Rho => Rho #makeMap(XS, #getTys(AL)) </tenv>
    <data> ... TC |-> (data TC (KSL:KSigList) AL:AltList) ... </data>

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


  // TODO: desugar fun TyList Ty correctly, also forall VarList KiList Ty (abs/lam rules)

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

  rule [ F:Val M1 M2 MS ] => [ [ F M1 ] M2 MS ]

  rule T <- (lam X1 X2 XS M) => T <- (lam X1 .VarList (lam X2 XS M))

  syntax Ty ::= #tyconHolder(K, K)

  rule #tyconHolder(MN, TC) => ( con (MN . TC) .TyList )

  rule boolean =>
    #tyconHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"),
                 #parseToken("TyCon@PLUTUS-CORE-SYNTAX", "Bool"))

  rule T <- (case M:NonValTm CLS) => (case M CLS) ~> T
  rule (case V:Val CLS) ~> T => T <- (case V CLS)


  syntax KiList ::= #getKinds(KSigList)                              [function]
  syntax TyVarList ::= #getTyVars(TyList)                            [function]
  syntax TyVarList ::= #getTyVars(AltList)                           [function]
  syntax TyList ::= #getTys(AltList)                                 [function]

  // TODO: make sure these are correct.
  // For example #getTyVars likely needs to be more general
  rule #getKinds(.KSigList) => .KiList
  rule #getKinds((X K) KSL) => K #getKinds(KSL)

  rule #getTyVars(.TyList) => .TyVarList
  rule #getTyVars(X:TyVar XS:TyList) => X #getTyVars(XS)

  rule #getTys(.AltList) => .TyList
  rule #getTys((C:Con T:Ty TL:TyList) ALS:AltList) => T #getTys((C TL) ALS)
  rule #getTys((C:Con .TyList) ALS:AltList) => #getTys(ALS)

  rule #getTyVars(.AltList) => .TyVarList
  rule #getTyVars((C:Con TL:TyList) ALS:AltList) => #getTyVars(TL) ++ #getTyVars(ALS)

  syntax TyVarList ::= TyVarList "++" TyVarList                      [function]
  rule .TyVarList ++ YS => YS
  rule (X XS) ++ YS => X (XS ++ YS)

  rule C in (C _)  AL:AltList => .K
  rule C in (C' _) AL:AltList => C in AL requires C =/=K C'

  syntax TyList ::= TyList "[" TyVarList ":=" TyList "]"             [function]
  syntax Map ::= #makeMap(TyVarList, TyList)                         [function]
  syntax Map ::= #makeMap(VarList, TyList)                           [function]
  syntax Ty ::= #subst(Ty, Map)                                      [function]

  rule #makeMap(.TyVarList, .TyList) => .Map
  rule #makeMap(TV:TyVar TVL, T TL) => (TV |-> T) #makeMap(TVL, TL)

  rule #makeMap(.VarList, .TyList) => .Map
  rule #makeMap(X:Var XL, T TL) => (X |-> T) #makeMap(XL, TL)

  rule #subst(T, M) => T [ M ]

  rule .TyList [ TVL := TL ] => .TyList
  rule (T TL1):TyList [ TVL := TL2 ]
    => #subst(T, #makeMap(TVL, TL2)) (TL1 [ TVL := TL2 ])

  // Types check Terms

  rule .TyList <-s .TmList => .K
  rule T TL <-s M ML => T <- M ~> TL <-s ML

  // Type checks list of clauses

  rule _ |- _ <-s .ClList => .K
  rule T1 |- T2 <-s CL CLS => T1 |- T2 <- CL ~> T1 |- T2 <-s CLS

endmodule
