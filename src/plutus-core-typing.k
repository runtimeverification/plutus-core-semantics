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
             | Ty "::" Ki                                  // `Ty` has kind `Ki`
             | QualC ":" ConValid
             | TyList "::" KiList
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
  syntax ConValid ::= "(" "forall" TyVarList KiList "(" "fun" TyList "[" QualTyC TyVarList "]" ")" ")"

  // TODO: need to do substitution in #getTys(AL) <-s MS
  rule
    <k>
      (con (MN . C:TyCon) TYS) <- (con QC:QualC MS:TmList)
        => QC : (forall #getTyVars(TYS) #getKinds(KSL)
                  (fun #getTys(AL) [ (MN . C) #getTyVars(TYS) ]))
        ~> #getTys(AL) <-s MS ...
    </k>
    <data> ... C |-> (data C (KSL:KSigList) AL:AltList) ... </data>

  // Checks: case.
  // TODO.

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
  rule (isa T M):Tm => T <- M ~> T

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
  rule (builtin addInt (integer) (integer)):Tm => (integer)
  rule (builtin multiplyInt (integer) (integer)):Tm => (integer)
  rule (builtin subtractInt (integer) (integer)):Tm => (integer)
  rule (builtin divideInt (integer) (integer)):Tm => (integer)
  rule (builtin remainderInt (integer) (integer)):Tm => (integer)

  rule (builtin lessthanInt (integer) (integer)):Tm => boolean
  rule (builtin lessthanEqualsInt (integer) (integer)):Tm => boolean
  rule (builtin greaterThanInt (integer) (integer)):Tm => boolean
  rule (builtin greaterThanEqualsInt (integer) (integer)):Tm => boolean
  rule (builtin equalsInt (integer) (integer)):Tm => boolean

  rule (builtin intToByteString (integer)):Tm => (bytestring)

  rule (builtin concatenate (bytestring) (bytestring)):Tm => (bytestring)

  rule (builtin take (integer) (bytestring)):Tm => (bytestring)
  rule (builtin drop (integer) (bytestring)):Tm => (bytestring)

  rule (builtin sha2_256 (bytestring)):Tm => (bytestring)
  rule (builtin sha3_256 (bytestring)):Tm => (bytestring)

  rule (builtin equalsByteString (bytestring) (bytestring)):Tm => (bytestring)

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
  /**                           OTHER JUDGMENTS                                */
  /*****************************************************************************/

  // con-valid-local
  // TODO.
  rule QC:QualC : CV:ConValid => .K

  /*****************************************************************************/
  /**                        HELPER FUNCTIONALITY                              */
  /*****************************************************************************/

  rule [ F:Val M1 M2 MS ] => [ [ F M1 ] M2 MS ]

  rule T <- (lam X1 X2 XS M) => T <- (lam X1 .VarList (lam X2 XS M))

  syntax Ty ::= #tyconHolder(K, K)

  rule #tyconHolder(MN, TC) => ( con (MN . TC) .TyList )

  rule boolean =>
    #tyconHolder(#parseToken("ModName@PLUTUS-CORE-SYNTAX", "Prelude"),
                 #parseToken("TyCon@PLUTUS-CORE-SYNTAX", "Boolean"))

  rule T <- (case M:NonValTm CLS) => (case M CLS) ~> T
  rule (case V:Val CLS) ~> T => T <- (case V CLS)

  syntax KiList ::= #getKinds(KSigList)
  syntax TyVarList ::= #getTyVars(TyList)
  syntax TyList ::= #getTys(AltList)

  rule #getKinds(.KSigList) => .KiList
  rule #getTyVars(.TyList) => .TyVarList
  rule #getTys(.AltList) => .TyList

  rule .TyList <-s .TmList => .K
  rule T TS <-s M MS => T <- M ~> TS <-s MS

endmodule
