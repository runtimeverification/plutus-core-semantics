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

  /* TODO: Add hook to ensure floats are always of the correct format */
  syntax PlFloat ::= r"[\\+\\-]?[0-9]+(\\.[0-9]+(([eE][+-]?[0-9]+)?|([eE][+-]?[0-9])+))" [token]
  syntax Float   ::= #toFloat(PlFloat)                                [function]

  // Exponent.
  syntax Exp     ::= r"[eE][+-]?[0-9]+"                                 [token]

  // Hex bytestring.
  syntax ByStr   ::= r"\\#[a-fA-F0-9]([a-fA-F0-9])*"                    [token]
                   | "#\".\""                                           [token]

  syntax Char    ::= r"\".\""                                           [token]

  // Arity.
  syntax Arity   ::= r"[0-9]+"                                          [token]

  // Variable.
  syntax Var     ::= r"[a-z][a-zA-Z0-9\\_']*"                           [token]
                   | symInt(Var)                                       [prefer]

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= ModName "." Name
  syntax QualC      ::= ModName "." Con

  syntax NameList   ::= List{Name, ""}
  syntax TmList     ::= List{Tm, ""}                           [klabel(tmList)]
  syntax ValList    ::= List{Val, ""}                         [klabel(valList)]
  syntax VarList    ::= List{Var, ""}                         [klabel(varList)]
  syntax QualCList  ::= List{QualC  ,  ""}

  syntax DefList    ::= List{Def    ,  ""}                    [klabel(defList)]

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
                      | #tmHolder(Float)
                      | "(" "lam" VarList Tm ")"                       [binder]

  // Case clause.
  syntax Cl         ::= "(" "cl" QualC "(" VarList ")" Tm ")"

  // Program.
  syntax Prg        ::= "(" "program" ModList ")"

  // Module.
  syntax Mod        ::= "(" "module" ModName ImpD ExpD LocD DefList ")"

  // Import declarations.
  syntax ImpD       ::= "(" "imported" ModList ")"

  // Export and local declarations.
  syntax ExpD       ::= "(" "exported" "(" CAList ")" "(" NameList ")" ")"
  syntax LocD       ::= "(" "local"    "(" CAList ")" "(" NameList ")" ")"

  syntax Def        ::= "(" "define" Name Tm ")"                    [strict(2)]

  // Lambda abstraction.
  syntax Val        ::= //closure(Map, Var, Tm)
                        // Constructed data.
                      "(" "con" QualC ValList ")"      [prefer, klabel(conVal)]
                      | InstrVal
                      | Int
                      | PlFloat
                      | ByStr

  syntax InstrVal   ::= "(" "failure" ")"
                      | "(" "txhash"  ")"
                      | "(" "blocknum" ")"
                      | "(" "blocktime" ")"
                      // TODO: fix test success.plcore
                      | "(" "success" Val ")"         [prefer, klabel(succVal)]
                      | "(" "bind" Val Var Tm ")"     [prefer, klabel(bindVal)]

  syntax KResult    ::= Val
  syntax KVariable  ::= Var

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toStringFloat(PlFloat)    [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
