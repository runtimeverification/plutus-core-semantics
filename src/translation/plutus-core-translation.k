require "../syntax.k"
require "iele-syntax.k"

module PLUTUS-CORE-TRANSLATION
  imports PLUTUS-CORE-SYNTAX
  imports IELE-SYNTAX

  configuration
    <T>
      <k> $PGM:DecList </k>
      <functions> .Map:Map </functions>
    </T>

// We translate each declaration iteratively
  rule D:Dec DS:DecList => D:Dec ~> DS:DecList

// Convert Lambdas
// ---------------

  syntax Tm ::= #closure(Name, Var, Tm, Map)                         [strict(3)]

// Top-level lambdas uses the names that the user has given them...
  rule (define N:Name (lam X:Var T:Tm))
   => #closure(N, X, T, .Map)

// ... whereas nested lambdas are assigned a fresh name
  rule #closure(N, X:Var, #freeLambda(Y, T), M)
    => #closure(N, X, #closure(#freshName, Y, T, M (X:Var |-> X:Var)), M)

  syntax Val ::= #freeLambda(Var, Tm)
  rule (lam X:Var T:Tm):Tm
    => #freeLambda(X, T)

  syntax Name ::= "#freshName"

// These are then converted to IELEFunctions (though we just abstract this for now.)

  syntax FunctionDefinition ::= #toIELEFunction(Name, FunctionParameters, Instructions)
  syntax LocalName ::= #stringToLocalName(String)

  // TODO: E needs to access env
  rule <k> #closure(N, X, I:Instructions, M) =>
           #allocBind(N, !N, M)
        ... </k>
       <functions> ...
             .Map
         => (!N:Int |-> #toIELEFunction(N, (#stringToLocalName("%env"), #varToLocalName(X)), I))
       </functions>

// `#allocBind` will allocate memory for a closure and initialize it with the correct "function pointer" and environment

  syntax Instructions ::= #allocBind(Name, Int, Map)

// Other constructs
// ----------------

// Currently, we put off implementing the details of IELE syntax:

  syntax Instructions ::= #toIELESyntax(Tm)
  syntax LocalName    ::= #varToLocalName(Var)
  syntax Val ::= Instructions | LocalName

  rule X:Var => #varToLocalName(X)
  rule ( builtin N:Name T1 T2 ) => #toIELESyntax(( builtin N T1 T2 ))

endmodule
