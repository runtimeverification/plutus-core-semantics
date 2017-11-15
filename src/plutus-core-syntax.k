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
                   | #stringToModName(String)                        [function]

  // Constructor name.
  syntax Con     ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]
                   | #stringToCon(String)                            [function]

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
  syntax QualC      ::= ModName "." Con

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
  syntax Tm         ::= "(" "abs" TyVar Tm ")"
                      | "(" "let" Tm Var Tm ")"                     [strict(1)]
                      | "(" "case" Tm ClList ")"                    [strict(1)]
                      | "(" "con" QualC TmList ")"
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(" "lam" VarList Tm ")"                       [binder]
                      | Val
                      | SynthTm

  syntax SynthTm    ::= Var
                      | QualN
                      | "(" "isa" Ty Tm ")"
                      | "(" "inst" Tm Ty ")"                        [strict(1)]
                      | "[" Tm TmList "]"                           [strict(1)]
                      | "(" "builtin" Name Tm ")"           [strict(2), prefer]
                      | "(" "builtin" Name Tm Tm ")"             [strict(2, 3)]
                      | "(" "compbuiltin" CBName ")"
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                      | Int
                      | ByStr
                      | SynthVal

  syntax Ki         ::= "(" "type" ")"
                      | "(" "fun" Ki Ki ")"

  syntax Ty         ::= TyVar
                      | "(" "fun" TyList Ty ")"
                      | "(" "con" QualC TyList ")"
                      | "(" "comp" Ty ")"
                      | "(" "forall" TyVarList KiList Ty ")"
                      | "(" "bytestring" ")"
                      | "(" "integer" ")"
                      | "(" "lam" TyVar Ki Ty ")"
                      | "[" Ty TyList "]"

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

  syntax Val        ::= "(" "con" QualC ValList ")"    [prefer, klabel(conVal)]
                      | "(" "failure" ")"
                      | "(" "success" Val ")"         [prefer, klabel(succVal)]
                      | SynthVal
                      | Ty

  syntax SynthVal   ::= "(" "bind" Val Var Tm ")"     [prefer, klabel(bindVal)]

  syntax KResult    ::= Val
  syntax KVariable  ::= Var | TyVar

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
