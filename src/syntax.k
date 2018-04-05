module PLUTUS-CORE-SYNTAX
  imports PLUTUS-CORE-COMMON

  syntax LowerName ::= r"[a-z][a-zA-Z0-9\\_']*"             [token, autoReject]
                     | #LowerId                             [token, autoReject]
  syntax UpperName ::= r"[A-Z][a-zA-Z0-9\\_']*"             [token, autoReject]
                     | #UpperId                             [token, autoReject]
endmodule

module PLUTUS-CORE-COMMON
  imports DOMAINS-SYNTAX
  imports BUILTIN-ID-TOKENS
  imports INT
  imports FLOAT
  imports STRING
  imports MAP

  /***************************************************************************/
  /***********************    LEXICAL GRAMMAR   ******************************/
  /***************************************************************************/
  syntax LowerName
  syntax UpperName

  syntax UnaryBuiltin ::= "integerToByteString"
                        | "sha3_256" | "sha2_256"
  syntax BinaryBuiltin ::= "addInteger" | "subtractInteger"
                         | "multiplyInteger" | "divideInteger" | "remainderInteger"
                         | "equalsInteger" | "lessThanInteger" | "greaterThanInteger"
                         | "lessThanEqualsInteger" | "greaterThanEqualsInteger"
                         | "concatenate" | "equalsByteString"
                         | "takeByteString" | "dropByteString"
  syntax BuiltinName ::= UnaryBuiltin | BinaryBuiltin

  // Name.
  syntax Name    ::= LowerName
                   | #stringToName(String)                           [function]
  syntax TyName  ::= LowerName

  // Builtin Name
  // CompBuiltin.
  syntax CBName  ::= LowerName

  // Module name.
  syntax ModName ::= UpperName
                   | #stringToModName(String)                        [function]
                   | ".ModName"
                   | "#MainModule"

  // Constructor name.
  syntax Con     ::= UpperName
                   | #stringToCon(String)                            [function]

  syntax TyCon   ::= UpperName

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
  syntax Var     ::= LowerName

  // Type Variable.
  syntax TyVar   ::= LowerName

  /***************************************************************************/
  /*****************************   GRAMMAR   *********************************/
  /***************************************************************************/

  // Qualified name and constructor.
  syntax QualN      ::= ModName "." Name                       [format(%1%2%3)]
  syntax QualTyN    ::= ModName "." TyName                     [format(%1%2%3)]
  syntax QualC      ::= ModName "." Con                        [format(%1%2%3)]
  syntax QualTyC    ::= ModName "." TyCon                      [format(%1%2%3)]

  syntax NameList   ::= List{Name, ""}
  syntax TmList     ::= List{Tm, ""}                           [klabel(tmList)]
  syntax TyList     ::= List{Ty, ""}                           [klabel(tyList)]
  syntax AltList    ::= List{Alt, ""}                         [klabel(altList)]
  syntax KiList     ::= List{Ki, ""}                           [klabel(kiList)]
  syntax KSigList   ::= List{KSig, ""}                       [klabel(ksigList)]
  syntax ValList    ::= List{Val, ""}                         [klabel(valList)]
  syntax VarList    ::= List{Var, ""}                         [klabel(varList)]
  syntax TyVarList  ::= List{TyVar, ""}                     [klabel(tyvarList)]
  syntax QualCList  ::= List{QualC, ""}

  // syntax DefList    ::= List{Def    ,  ""}                    [klabel(defList)]
  syntax DecList    ::= List{Dec, ""}                         [klabel(declist)]

  syntax ModList    ::= List{Mod, ""}                         [klabel(modList)]
  syntax ModNmList  ::= List{ModName, ""}                 [klabel(modNameList)]
  syntax ClList     ::= List{Cl, ""}                           [klabel(clList)]

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
                      | "(" "con" QualC TmList ")"       [format(%1%2 %3 %4%5)]
                        // Built-in function.
                      | "(" "builtin" BuiltinName Tm ")"    [strict(2), prefer]
                      | "(" "builtin" BuiltinName Tm Tm ")"      [strict(2, 3)]
                      | "(" "compbuiltin" CBName ")"
                      | "(" "success" Tm ")"                        [strict(1)]
                      | "(" "bind" Tm Var Tm ")"                    [strict(1)]
                      | Val

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

  syntax Def        ::= "(" "define" Name Val ")"

  syntax TDec       ::= "(" "type" Name Ty ")"

  syntax DDec       ::= "(" "data" TyCon "(" KSigList ")" AltList ")"

  // Type annotations for terms. Missing from prelude?
  syntax MDec       ::= "(" "declare" Name Ty ")"

  syntax ValResult  ::= "(" "con" QualC ValList ")"    [prefer, klabel(conVal),
                                                          format(%1%2 %3 %4%5)]
                      | "(" "failure" ")"
                      | "(" "success" Val ")"         [prefer, klabel(succVal)]
                      | "(" "bind" Val Var Tm ")"     [prefer, klabel(bindVal)]
                      | Int
                      | ByStr

  syntax Val        ::= ValResult
                      | "(" "lam" VarList Tm ")"                       [binder]

  syntax KResult    ::= ValResult
  syntax KVariable  ::= Var | TyVar

  syntax String     ::= #toStringVerbatim(ByStr)   [ function
                                                   , hook(STRING.token2string)]
                      | #toString(ByStr)                             [function]

endmodule
