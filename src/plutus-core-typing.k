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
  rule (fun S T) :: (type):Ki => S :: (type):Ki ~> T :: (type):Ki

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
  rule (integer)<:Ty :: (type):Ki => .K

  // bytestring
  rule (bytestring)<:Ty :: (type):Ki => .K

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

  syntax K ::= "(" "forall" TyVarList KiList "(" "fun" TyList "[" QualTyC TyVarList "]" ")" ")"

  // Checks: con.
  // rule
  //   <k> (con QTC TYS) <- (con QC MS) =>
  //   <data> ... C |-> (data C (KSL) AL) ... </data>

  // TODO: the following will change.
  // rule
  //   <k>
  //     ((con (MN . C) TYS) <- (con QC MS))
  //       => checksCon(QC, (forall #getTyVars(TYS) #getKinds(KSL)
  //                          (fun #getTyVars(TYS)
  //                            (con (MN . C) #getTyVars(TYS)))))
  //       ~> checksList(TYS, MS)
  //   </k>
  //   <data> ... C |-> (data C (KSL) AL) ... </data>

  // Checks: case.
  // TODO.

  // Checks: success.
  rule (comp T) <- (success M) => M <- T

  // Checks: failure.
  rule (comp T) <- (failure)<:Val => .K

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
  rule I:Int => (integer)<:Ty

  // Synthesis: bytestringval.
  rule B:ByStr => (bytestring)<:Ty

  // builtins
  rule (builtin addInt (integer)<:Ty (integer)<:Ty):Tm => (integer)<:Ty
  rule (builtin multiplyInt (integer)<:Ty (integer)<:Ty):Tm => (integer)<:Ty
  rule (builtin subtractInt (integer)<:Ty (integer)<:Ty):Tm => (integer)<:Ty
  rule (builtin divideInt (integer)<:Ty (integer)<:Ty):Tm => (integer)<:Ty
  rule (builtin remainderInt (integer)<:Ty (integer)<:Ty):Tm => (integer)<:Ty

  rule (builtin lessthanInt (integer)<:Ty (integer)<:Ty):Tm => boolean
  rule (builtin lessthanEqualsInt (integer)<:Ty (integer)<:Ty):Tm => boolean
  rule (builtin greaterThanInt (integer)<:Ty (integer)<:Ty):Tm => boolean
  rule (builtin greaterThanEqualsInt (integer)<:Ty (integer)<:Ty):Tm => boolean
  rule (builtin equalsInt (integer)<:Ty (integer)<:Ty):Tm => boolean

  rule (builtin intToByteString (integer)<:Ty):Tm => (bytestring)<:Ty

  rule (builtin concatenate (bytestring)<:Ty (bytestring)<:Ty):Tm => (bytestring)<:Ty

  rule (builtin take (integer)<:Ty (bytestring)<:Ty):Tm => (bytestring)<:Ty
  rule (builtin drop (integer)<:Ty (bytestring)<:Ty):Tm => (bytestring)<:Ty

  rule (builtin sha2_256 (bytestring)<:Ty):Tm => (bytestring)<:Ty
  rule (builtin sha3_256 (bytestring)<:Ty):Tm => (bytestring)<:Ty

  rule (builtin equalsByteString (bytestring)<:Ty (bytestring)<:Ty):Tm => (bytestring)<:Ty

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
                 #parseToken("TyCon@PLUTUS-CORE-SYNTAX", "Boolean"))

  syntax KiList ::= #getKinds(KSigList)
  syntax TyVarList ::= #getTyVars(TyList)
  syntax TyList ::= #getTys(AltList)

  rule #getKinds(.KSigList) => .KiList
  rule #getTyVars(.TyList) => .TyVarList
  rule #getTys(.AltList) => .TyList

endmodule
