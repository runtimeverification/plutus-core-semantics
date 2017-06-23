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
  syntax ValList    ::= "TODO"
  syntax VarList    ::= "TODO"
  syntax QualCList  ::= List{QualC  ,  " "}
  syntax DefList    ::= List{Def    ,  " "}
  syntax ModList    ::= List{Mod    ,  " "}
  syntax ClList     ::= List{Cl     ,  " "}

  syntax ConArList  ::= ""
                      | "(" Con Arity ")" ConArList

  syntax Tm         ::= "(" "let" Tm Var Tm ")"               [seqstrict(1, 3)]
                        // Application.
                      | "[" Tm Tm "]"                         [seqstrict(1, 2)]
                      | "(" "case" Tm QualCList ")" // Case.
                       // Built-in function.
                      | "(" "builtin" Name "(" Tm ")" ")"   [strict(2), prefer]
                      | "(" "builtin" Name "(" Tm Tm ")" ")"  [seqstrict(2, 3)]
                      // Lambda abstraction.
                      | Val


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

  syntax Def          ::= "(" "define" Name Tm ")"                  [strict(2)]

  // Lambda abstraction.
  syntax Val          ::= Var
                        | "(" "lam" Var Tm ")"                         [binder]
                        | Instr
                        // Constructed data.
                        | "(" "con" QualC TmList ")"
                        | Int
                        | Float
                        | ByStr

  syntax Instr        ::= "(" "success" Tm ")"                      [strict(1)]
                        | "(" "failure" ")"                          [failureV]
                        | "(" "txhash"  ")"
                        | "(" "blocknum" ")"
                        | "(" "blocktime" ")"
                        | "(" "bind" Tm Var Tm ")"

  syntax KResult      ::= Val
  syntax KVariable    ::= Var

endmodule
