module PLUTUS-CORE-SYNTAX
  imports INT
  imports FLOAT
  imports SUBSTITUTION
  /*import FLOAT-SYNTAX*/

  /***************************************************************************/
  /***********************    LEXICAL GRAMMAR   ******************************/
  /***************************************************************************/

  // Name.
  syntax Name    ::= r"[a-z][a-zA-Z0-9\\_']*"                    [token, avoid]

  // Module name.
  syntax ModName ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]
                   | ".ModName"

  // Constructor name.
  syntax Con     ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]

  // Integer literal.
  // NOTE: K's builtin `Int` sort is based on exactly the same regular
  // expression as that in the Plutus Core specification.

  /* TODO: Make sure that K's built-in Float is compatible with PlFloat*/
   /*syntax Float ::= "TODO"*/

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

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= ModName "." Name
                      | #qualN(ModName, Name)                        [function]

  syntax QualC      ::= ModName "." Con

  syntax NameList   ::= List{Name, ""}
  syntax TmList     ::= List{Tm, ""}                          [klabel(tmList)]
  syntax ValList    ::= List{Val, ""}                        [klabel(valList)]
  syntax VarList    ::= List{Var, ""}
  syntax QualCList  ::= List{QualC  ,  ""}

  syntax DefList    ::= List{Def    ,  ""}                   [klabel(defList)]

  syntax ModList    ::= List{ModName , ""}
  syntax ClList     ::= List{Cl     ,  ""}

  syntax ConArity   ::= "(" Con Arity ")"
  syntax CAList     ::= List{ConArity, ""}

  syntax Tm         ::= Var
                      | QualN
                      | "(" "let" Tm Var Tm ")"                     [strict(1)]
                        // Application.
                      | "[" TmList "]"
                      | "(" "case" Tm ClList ")"                    [strict(1)]
                      | "(" "con" QualC TmList ")"                  [strict(2)]
                        // Built-in function.
                      | "(" "builtin" Name Tm ")"
                                                            [strict(2), prefer]
                      | "(" "builtin" Name Tm Tm ")"
                                                                 [strict(2, 3)]
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                        // Lambda abstraction.
                      | Val
                      | #tmHolder(Float)

  // Case clause.
  syntax Cl           ::= "(" "cl" QualC "(" VarList ")" Tm ")"

  // Program.
  syntax Prg          ::= "(" "program" ModList ")"

  // Module.
  syntax Mod          ::= "(" "module" ModName ImpD ExpD LocD DefList ")"

  // Import declarations.
  syntax ImpD         ::= "(" "imported" ModList ")"

  // Export and local declarations.
  syntax ExpD         ::= "(" "exported" "(" CAList ")" "(" NameList ")" ")"
  syntax LocD         ::= "(" "local"    "(" CAList ")" "(" NameList ")" ")"

  syntax Def          ::= "(" "define" Name Tm ")"                  [strict(2)]

  // Lambda abstraction.
  syntax Val          ::= "(" "lam" Var Tm ")"                         [binder]
                          // Constructed data.
                        | "(" "con" QualC ValList ")"                  [prefer]
                        | InstrVal
                        | Int
                        | Float
                        | ByStr

  syntax InstrVal   ::= "(" "success" Val ")"
                      | "(" "failure" ")"
                      | "(" "txhash"  ")"
                      | "(" "blocknum" ")"
                      | "(" "blocktime" ")"
                      | "(" "bind" Val Var Tm ")"

  syntax KResult      ::= Val
  syntax KVariable    ::= Var

  syntax String       ::= toStringVerbatim(ByStr)  [ function
                                                   , hook(STRING.token2string)]
                        | toString(ByStr)                            [function]

endmodule
