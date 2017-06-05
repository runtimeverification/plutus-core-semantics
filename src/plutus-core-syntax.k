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
  syntax Integer ::= r"[+-]?[0-9]+"

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
  syntax QualN   ::= Mod "." Name
  syntax QualC   ::= Mod "." Con

  syntax TmList     ::= List{Tm, " "}
  syntax QualCList  ::= List{QualC, " "}

  syntax Tm      ::= Var
                   | "(" "let" Tm Var Tm ")" // Local declaration.
                   | "(" "lam" Var Tm ")" // λ abstraction
                   | "[" Tm Tm "]" // Application
                   | "(" "con" QualC TmList ")" // Constructed data
                   | "(" "case" Tm QualCList ")" // Case
                   | "(" "success" Tm ")" // Success
                   | "(" "failure" ")" // Failure
                   | "(" "txhash"  ")" // Transaction hash
                   | "(" "blocknum" ")" // Block number
                   | "(" "blocktime" ")" // Block time
                   | "(" "bind" Tm Var Tm ")" // Computation bind
                   | Integer // Primitive integer
                   | Float   // Primitive float
                   | ByStr   // Primitive bytestring
                   | "(" "builtin" Name TmList ")" // Built-in function

endmodule
