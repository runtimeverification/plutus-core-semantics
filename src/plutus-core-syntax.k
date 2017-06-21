require "domains.k"

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
  syntax Mod     ::= r"[A-Z][a-zA-Z0-9\\_']*"                    [token, avoid]

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
  syntax ByStr   ::= r"#([a-fA-F0-9][a-fA-F0-9])+"                      [token]
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
  syntax QualN      ::= Mod "." Name
  syntax QualC      ::= Mod "." Con

  syntax NameList   ::= List{Name   ,  " "}
  syntax TmList     ::= "TODO"
  syntax ValList    ::= List{Val    ,  " "}
  syntax VarList    ::= "TODO"
  syntax QualCList  ::= List{QualC  ,  " "}
  syntax DefList    ::= List{Def    ,  " "}
  syntax ModList    ::= List{Mod    ,  " "}
  syntax ClList     ::= List{Cl     ,  " "}

  syntax ConArList  ::= ""
                      | "(" Con Arity ")" ConArList

  syntax Tm         ::= Var
                        // Local declaration.
                      | "(" "let" Tm Var Tm ")"
                        // Application.
                      | "[" Tm Tm "]"
                      | "(" "case" Tm QualCList ")" // Case.
                       // Built-in function.
                      | "(" "builtin" Name "(" Tm ")" ")"   [strict(2), prefer]
                      | "(" "builtin" Name "(" Tm Tm ")" ")"     [strict(2, 3)]
                      // Lambda abstraction.
                      | "(" "lam" Var Tm ")"           [klabel(lamExp), binder]
                      | "(" "con" QualC TmList ")" // Constructed data.
                      | "(" "success" Tm ")" // Success.
                      | "(" "failure" ")" // Failure.
                      | "(" "txhash"  ")" // Transaction hash.
                      | "(" "blocknum" ")" // Block number.
                      | "(" "blocktime" ")" // Block time.
                      | "(" "bind" Tm Var Tm ")" // Computation bind.
                      | Int // Primitive integer.
                      | Float   // Primitive float.
                      | ByStr   // Primitive bytestring.


  // Case clause.
  syntax Cl           ::= "(" "cl" QualC "(" VarList ")" Tm ")"

  // Program.
  syntax Prg          ::= "(" "program" ModList ")"

  // Module.
  syntax Mod          ::= "(" "module" Mod ImpD ExpD LocD DefList ")"

  // Import declarations.
  syntax ImpD         ::= "(" "imported" ModList ")"

  // Export and local declarations.
  syntax ExpD         ::= "(" "exported" "(" ConArList ")" "(" NameList ")" ")"
  syntax LocD         ::= "(" "local"    "(" ConArList ")" "(" NameList ")" ")"

  syntax Def          ::= "(" "define" Name Val ")"

  // Lambda abstraction.
  syntax Val          ::= "(" "lam" Var Tm ")" [klabel(lamV)]
                        // Constructed data.
                        | "(" "con" QualC TmList ")" [klabel(conV)]
                        | "(" "success" Tm ")" [klabel(successV)] // Success.
                        | "(" "failure" ")" [klabel(failureV)] // Failure.
                        | "(" "txhash"  ")" [klabel(txhashV)]// Transaction hash.
                        | "(" "blocknum" ")" [klabel(blocknumV)] // Block number.
                        | "(" "blocktime" ")" [klabel(blocktimeV)] // Block time.
                        | "(" "bind" Tm Var Tm ")" [klabel(bindV)] // Computation bind.
                        | Int [klabel(intV)]// Primitive integer.
                        | Float [klabel(floatV)]  // Primitive float.
                        | ByStr  [klabel(byStrV)]// Primitive bytestring.

  syntax KVariable    ::= Var
  syntax KResult      ::= Val

endmodule
