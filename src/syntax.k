module PLUTUS-CORE-SYNTAX
  imports INT
  imports FLOAT
  imports STRING
  imports MAP

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
                   | #stringToModName(String)                        [function]
                   | ".ModName"

  // Constructor name.
  syntax Con     ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]
                   | #stringToCon(String)                            [function]

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
  syntax DecList    ::= List{Dec, ""}                         [klabel(declist)]

  syntax ModList    ::= List{Mod, ""}                         [klabel(modList)]
  syntax ModNmList  ::= List{ModName, ""}                 [klabel(modNameList)]
  syntax ClList     ::= List{Cl ,  ""}                         [klabel(clList)]

  syntax ConList    ::= List{Con, ""}                         [klabel(conList)]

  // TODO: current spec still has floats?

  syntax Tm         ::= Var
                      | QualN
                      | "(" "isa" Tm Ty ")"
                      | "(" "abs" Var Tm ")"
                      | "(" "inst" Tm Ty ")"
                      // TODO: remove let?
                      | "(" "let" Tm Var Tm ")"                     [strict(1)]
                        // Application.
                      | "[" Tm TmList "]"                           [strict(1)]
                      | "(" "case" Tm ClList ")"                    [strict(1)]
                      | "(" "con" QualC TmList ")"
                        // Built-in function.
                      | "(" "builtin" Name Tm ")"
                                                            [strict(2), prefer]
                      | "(" "builtin" Name Tm Tm ")"
                                                                 [strict(2, 3)]
                      | "(" "compbuiltin" CBName ")"
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                      | Val
                      | "(" "lam" VarList Tm ")"                       [binder]

  syntax Ki         ::= "(" "type" ")"
                      | "(" "fun" Ki Ki ")"

  syntax Ty         ::= TyVar
                      | "(" "fun" Ty Ty ")"
                      | "(" "con" QualTyC TyList ")"
                      | "(" "comp" Ty ")"
                      | "(" "forall" TyVar Ki Ty ")"
                      | "(bytestring)"
                      | "(integer)"
                      | "(" "lam" TyVar Ki Ty ")"
                      | TyApp

  syntax TyApp      ::= "[" Ty TyList "]"

  // Case clause.
  syntax Cl         ::= "(" QualC "(" VarList ")" Tm ")"

  // Kind signature.
  syntax KSig       ::= "(" TyVar Ki ")"

  // Alternative.
  syntax Alt        ::= "(" Con TyList ")"

  // Program.
  syntax Prg        ::= "(" "program" ModList ")"

  // Module.
  syntax Mod        ::= "(" "module" ModName ImpD ExpD DecList ")"

  // Import declarations.
  syntax ImpD       ::= "(" "import" ModNmList ")"

  // Export and local declarations.
  syntax ExpD       ::= "(" "export" "(" TyExpList ")" "(" NameList ")" ")"

  syntax TyExport   ::= TyName
                      | "(" TyCon "(" ConList ")" ")"

  syntax TyExpList  ::= List{TyExport, ""}               [klabel(tyExportList)]

  syntax Dec        ::= Def
                      | TDec
                      | MDec
                      | DDec

  syntax Def        ::= "(" "define" Name Tm ")"                    [strict(2)]

  syntax TDec       ::= "(" "type" Name Ty ")"

  syntax DDec       ::= "(" "data" TyCon "(" KSigList ")" AltList ")"

  // Type annotations for terms. Missing from prelude?
  syntax MDec       ::= "(" "declare" Name Ty ")"

  syntax Val        ::= "(" "con" QualC ValList ")"    [prefer, klabel(conVal)]
                      | "(" "failure" ")"
                      | "(" "success" Val ")"         [prefer, klabel(succVal)]
                      | "(" "bind" Val Var Tm ")"     [prefer, klabel(bindVal)]
                      | Int
                      | ByStr

  syntax KResult    ::= Val
  syntax KVariable  ::= Var | TyVar

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
