module PLUTUS-CORE-SYNTAX
  imports INT
  imports FLOAT

  /***************************************************************************/
  /***********************    LEXICAL GRAMMAR   ******************************/
  /***************************************************************************/

  // Name.
  syntax Name    ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]

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

  // Variable.
  syntax Var     ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]
                   | symInt(Var)                                       [prefer]

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= ModName "." Name
  syntax QualC      ::= ModName "." Con

  syntax NameList   ::= List{Name, ""}
  syntax TmList     ::= List{Tm, ""}                           [klabel(tmList)]
  syntax TyList     ::= List{Ty, ""}                           [klabel(tyList)]
  syntax KiList     ::= List{Ki, ""}                           [klabel(kiList)]
  syntax ValList    ::= List{Val, ""}                         [klabel(valList)]
  syntax VarList    ::= List{Var, ""}                         [klabel(varList)]
  syntax QualCList  ::= List{QualC  ,  ""}

  // syntax DefList    ::= List{Def    ,  ""}                    [klabel(defList)]
  syntax DecList    ::= List{Dec, ""}

  syntax ModList    ::= List{ModName , ""}
  syntax ClList     ::= List{Cl     ,  ""}

  syntax ConArity   ::= "(" Con Arity ")"
  syntax CAList     ::= List{ConArity, ""}

  syntax Tm         ::= Var
                      | QualN
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
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                      | Val
                      | "(" "lam" VarList Tm ")"                       [binder]

  syntax Ki         ::= "type"
                      | "(" "fun" Ki Ki ")"

  syntax Ty         ::= Var
                      | "(" "fun" Ty Ty ")"
                      | "(" "con" QualC TyList ")"
                      | "(" "comp" Ty ")"
                      | "(" "forall" Var Ki Ty ")"
                      | "bytestring"
                      | "integer"
                      | "(" "lam" Var Ki Ty ")"
                      | "[" Ty Ty "]"

  // Case clause.
  syntax Cl         ::= "(" "cl" QualC "(" VarList ")" Tm ")"

  // Program.
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

  syntax Def        ::= "(" "define" Name Tm ")"                    [strict(2)]

  syntax TDec       ::= "(" "type" Name Ty ")"

  // Type annotations for terms.
  syntax MDec       ::= "(" "declare" Name Ty ")"

  syntax Val        ::= "(" "con" QualC ValList ")"    [prefer, klabel(conVal)]
                      | InstrVal
                      | Int
                      | ByStr

  syntax InstrVal   ::= "(" "failure" ")"
                      | "(" "txhash"  ")"
                      | "(" "blocknum" ")"
                      | "(" "blocktime" ")"
                      | "(" "success" Val ")"         [prefer, klabel(succVal)]
                      | "(" "bind" Val Var Tm ")"     [prefer, klabel(bindVal)]

  syntax KResult    ::= Val
  syntax KVariable  ::= Var

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
