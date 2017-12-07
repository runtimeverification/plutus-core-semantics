module PLUTUS-CORE-SYNTAX
  imports INT
  imports FLOAT
  imports SUBSTITUTION

  /***************************************************************************/
  /***********************    LEXICAL GRAMMAR   ******************************/
  /***************************************************************************/

  // Name.
  syntax Name    ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]
  syntax TyName  ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]

  // CompBuiltin.
  syntax CBName  ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]

  // Module name.
  syntax ModName ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]
                   | ".ModName"

  // Constructor name.
  syntax Con     ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]
  syntax TyCon   ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]

  // Integer literal.
  // NOTE: K's builtin `Int` sort is based on exactly the same regular
  // expression as that in the Plutus Core specification.

  // Exponent.
  syntax Exp     ::= r"[eE][+-]?[0-9]+"                                 [token]

  // Hex bytestring.
  syntax ByStr   ::= r"\\#[a-fA-F0-9]([a-fA-F0-9])*"                    [token]
                   | "#\".\""                                           [token]

  syntax Char    ::= r"\".\""                                           [token]

  // Arity.
  syntax Arity   ::= r"[0-9]+"                                          [token]

  // TODO: Make sure preferring Var over TyVar is ok.
  // Variable.
  syntax Var     ::= r"[a-z][a-zA-Z0-9\\_']*"                           [token]

  // Type Variable.
  syntax TyVar     ::= r"[a-z][a-zA-Z0-9\\_']*"                  [token, avoid]

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= ModName "." Name
  syntax QualTyN    ::= ModName "." TyName
  syntax QualC      ::= ModName "." Con
  syntax QualTyC    ::= ModName "." TyCon

  syntax NameList   ::= List{Name, ""}
  syntax TmList     ::= List{Tm, ""}                           [klabel(tmList)]
  syntax TyList     ::= List{Ty, ""}                           [klabel(tyList)]
  syntax AltList    ::= List{Alt, ""}                         [klabel(altList)]
  syntax KiList     ::= List{Ki, ""}                           [klabel(kiList)]
  syntax KSigList   ::= List{KSig, ""}                       [klabel(ksigList)]
  syntax ValList    ::= List{Val, ""}                         [klabel(valList)]
  syntax VarList    ::= List{Var, ""}                         [klabel(varList)]
  syntax TyVarList  ::= List{TyVar, ""}                     [klabel(tyvarList)]
  syntax QualCList  ::= List{QualC  ,  ""}

  // syntax DefList    ::= List{Def    ,  ""}                    [klabel(defList)]
  syntax DecList    ::= List{Dec, ""}

  syntax ModList    ::= List{ModName , ""}
  syntax ClList     ::= List{Cl     ,  ""}

  syntax ConArity   ::= "(" Con Arity ")"
  syntax CAList     ::= List{ConArity, ""}

  // TODO: current spec still has floats?
  syntax NonValTm   ::= "(" "abs" TyVar Tm ")"
                      | "(" "let" Tm Var Tm ")"                     [strict(1)]
                      | "(" "case" Tm ClList ")"                    [strict(1)]
                      // TODO: prefer con (terms) to con (types)?
                      | "(" "con" QualC TmList ")"                     [prefer]
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(failure)"
                      | "(" "lam" VarList Tm ")"                       [binder]
                      | SynthTm

  syntax Tm         ::= NonValTm
                      | Val

  syntax SynthTm    ::= Var
                      | QualN
                      | "(" "isa" Ty Tm ")"
                      | "(" "inst" Tm Ty ")"                        [strict(1)]
                      // TODO: distinguish NeTmList and TmList
                      // TODO: prefer term app over type app?
                      | "[" Tm TmList "]"                   [strict(1), prefer]
                      | "(" "builtin" Name Tm ")"           [strict(2), prefer]
                      | "(" "builtin" Name Tm Tm ")"             [strict(2, 3)]
                      | "(" "compbuiltin" CBName ")"
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                      | Int
                      | ByStr

  syntax Ki         ::= "(" "type" ")"
                      | "(" "fun" Ki Ki ")"

  syntax Ty         ::= TyVar
                      | "(" "fun" Ty Ty ")"
                      | "(" "con" QualTyC TyList ")"
                      | "(" "comp" Ty ")"
                      | "(" "forall" TyVar Ki Ty ")"
                      | "(" "bytestring" ")"
                      | "(" "integer" ")"
                      | "(" "lam" TyVar Ki Ty ")"
                      | TyApp

  syntax TyApp      ::= "[" Ty TyList "]"                           [strict(1)]

  // Case clause.
  syntax Cl         ::= "(" QualC "(" VarList ")" Tm ")"

  // Kind signature.
  syntax KSig       ::= "(" TyVar Ki ")"

  // Alternative.
  syntax Alt        ::= "(" Con TyList ")"

  // Program.
  // TODO: make program a Prg
  syntax Prg        ::= "(" "program" ModList ")"

  // Module.
  syntax Mod        ::= "(" "module" ModName ImpD ExpD LocD DecList ")"

  // Import declarations.
  syntax ImpD       ::= "(" "imported" ModList ")"

  // Export and local declarations.
  syntax ExpD       ::= "(" "exported" "(" CAList ")" "(" NameList ")" ")"
  syntax LocD       ::= "(" "local"    "(" CAList ")" "(" NameList ")" ")"

  syntax Dec        ::= Def
                      | TDec
                      | MDec
                      | DDec

  syntax Def        ::= "(" "define" Name Tm ")"

  syntax TDec       ::= "(" "type" Name Ty ")"

  syntax DDec       ::= "(" "data" Con "(" KSigList ")" AltList ")"

  // Type annotations for terms. Missing from prelude?
  syntax MDec       ::= "(" "declare" Name Ty ")"

  syntax Val        ::= Ty

  syntax KResult    ::= Val
  syntax KVariable  ::= Var | TyVar

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
