require "../syntax.k"
require "iele-syntax.k"

module PLUTUS-CORE-TRANSLATION
  imports PLUTUS-CORE-SYNTAX
  imports IELE-SYNTAX

  configuration
    <T>
      <k> $PGM:Dec </k>
    </T>

// Convert Lambdas
// ---------------

  syntax Tm ::= #closure(Name, Var, Tm, Map)                         [strict(3)]

// Top-level lambdas uses the names that the user has given them
  rule (define N:Name (lam X:Var T:Tm))
   => #closure(N, X, T, .Map)

// Whereas nested lambdas are assigned a fresh name
  rule #closure(N, X:Var, #freeLambda(Y, T), M)
    => #closure(N, X, #closure(#freshName, Y, T, M (X:Var |-> X:Var)), M)

  syntax Val ::= #freeLambda(Var, Tm)
  rule (lam X:Var T:Tm):Tm
    => #freeLambda(X, T)

  syntax Name ::= "#freshName"

// Other constructs
// ----------------

// Currently, we put off implementing the details of IELE syntax:

  syntax Val ::= #toIELESyntax(Tm)

  rule X:Var => #toIELESyntax(X)
  rule ( builtin N:Name T1 T2 ) => #toIELESyntax(( builtin N T1 T2 ))

endmodule
