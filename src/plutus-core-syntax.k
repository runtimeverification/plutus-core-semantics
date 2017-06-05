module PLUTUS-CORE-SYNTAX

  /***************************************************************************/
  /***********************    LEXICAL GRAMMAR   ******************************/
  /***************************************************************************/

  // Name.
  syntax Name    ::= r"[a-z][a-zA-Z0-9\\_']*"                           [token]

  // Module name.
  syntax Mod     ::= r"[A-Z][a-zA-Z0-9\\_']*"                           [token]

  // Constructor name.
  syntax Con     ::= r"[A-Z][a-zA-Z0-9\\_']*"                           [token]

  // Integer literal.
  syntax Integer ::= r"[0-9]+"                                          [token]

  // Float literal.
  syntax Float   ::= r"[+-]?[0-9]+(.[0-9]+e{0,1})e"                     [token]

  // Exponent.
  syntax Exp     ::= r"[eE][+-]?[0-9]+"                                 [token]

  // Hex bytestring.
  syntax ByStr   ::= r"#([a-fA-F0-9][a-fA-F0-9])+"                      [token]
                   | "#\".\""                                           [token]

  syntax Char    ::= r"\".\""                                           [token]

  syntax Arity   ::= r"[0-9]+"                                          [token]

  // Variable.
  syntax Var     ::= r"[a-z][a-zA-Z0-9\\_']*"                           [token]

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= Mod "." Name
  syntax QualC      ::= Mod "." Con

  syntax TmList     ::= List{Tm     ,  " "}
  syntax VarList    ::= List{Var    ,  " "}
  syntax QualCList  ::= List{QualC  ,  " "}
  syntax DefList    ::= List{Def    ,  " "}
  syntax ModList    ::= List{Mod    ,  " "}

  syntax Tm         ::= Var
                        // Local declaration.
                      | "(" "let" Tm Var Tm ")"
                        // Application.
                      | "[" Tm Tm "]"                             [klabel(app)]
                      | "(" "case" Tm QualCList ")" // Case.
                       // Built-in function.
                      | "(" "builtin" Name "(" TmList ")" ")"
                      | Val

// Case clause.
syntax Cl           ::= "(" "cl" QualC "(" VarList ")" Tm ")"

// Program.
syntax Prg          ::= "(" "program" ModList ")"

// Module.
syntax Mod          ::= "(" "module" Mod ImpD ExpD LocD DefList ")"

// Import declarations.
syntax ImpD         ::= "(" "imported" ModList ")"

// Export declarations.
syntax ExpD         ::= "TODO"

syntax LocD         ::= "TODO"

syntax Def          ::= "(" "define" Name Val ")"

syntax Val          ::= "(" "lam" Var Tm ")" // λ abstraction.
                      | "(" "con" QualC TmList ")" // Constructed data.
                      | "(" "success" Tm ")" // Success.
                      | "(" "failure" ")" // Failure.
                      | "(" "txhash"  ")" // Transaction hash.
                      | "(" "blocknum" ")" // Block number.
                      | "(" "blocktime" ")" // Block time.
                      | "(" "bind" Tm Var Tm ")" // Computation bind.
                      | Integer // Primitive integer.
                      | Float   // Primitive float.
                      | ByStr   // Primitive bytestring.

endmodule
