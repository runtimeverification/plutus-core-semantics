require "../syntax.k"
require "iele-syntax.k"

module PLUTUS-CORE-TRANSLATION
  imports PLUTUS-CORE-SYNTAX

// Pull off some stunts to avoid ambiguities between `.K` and IELEName
  imports IELE-COMMON
  syntax IeleName ::= r"(?<![A-Za-z0-9\\_\\-\\$])[a-zA-Z\\_\\-\\$][0-9a-zA-Z\\_\\-\\$]*" [token, notInRules, prec(3)]

  configuration
    <T>
      <k> $PGM:DecList </k>
      <functions> .Map:Map </functions>
    </T>

// We translate each declaration iteratively
  rule D:Dec DS:DecList => D:Dec ~> DS:DecList

// Helpers
// -------
  syntax LocalName ::= #stringToLocalName(String)                   [function]
  rule #stringToLocalName(S) => #parseToken("LocalName@IELE-COMMON", "%" +String S)

  syntax GlobalName ::= #stringToGlobalName(String)                   [function]
  rule #stringToGlobalName(S) => #parseToken("GlobalName@IELE-COMMON", "@" +String S)

  syntax String       ::= #varToString(Var)       [function, hook(STRING.token2string)]
  syntax LocalName    ::= #varToLocalName(Var)    [function]
  rule #varToLocalName(V:Var) => #stringToLocalName(#varToString(V))

  syntax LocalName ::= #intToLocalName(Int)       [function]
  rule #intToLocalName(N)
    => #stringToLocalName("fresh" +String Int2String(N))

  syntax String ::= "#freshString"                            [function]
  rule #freshString:String
    => "fresh" +String Int2String(!N)

// Convert Lambdas
// ---------------

  syntax Tm ::= #closure(String, Var, Tm, Map)                         [strict(3)]

// Top-level lambdas uses the names that the user has given them...
  rule (define N:Name (lam X:Var T:Tm))
    => #closure(#freshString, X, T, .Map)

// ... whereas nested lambdas are assigned a fresh name
  rule #closure(N, X:Var, #freeLambda(Y, T), M)
    => #closure(N, X, #closure(#freshString, Y, T, (X:Var |-> X:Var) M), M)

  syntax Val ::= #freeLambda(Var, Tm)
  rule (lam X:Var T:Tm):Tm
    => #freeLambda(X, T)

// These are then converted to IELEFunctions (though we just abstract this for now.)

  syntax FunctionDefinition ::= #toIELEFunction(String, FunctionParameters, Instructions) [function]
  rule #toIELEFunction(N, PARAMS, I)
    => define #stringToGlobalName(N) (PARAMS) { I .LabeledBlocks }

  // TODO: E needs to access env
  rule <k> #closure(NAME:String, X, I:Instructions, M) =>
           #allocBind(NAME, !N, M)
        ... </k>
       <functions> ...
             .Map
         => (!N:Int |-> #toIELEFunction(NAME, (#stringToLocalName("env"), #varToLocalName(X)), I))
       </functions>

// `#allocBind` will allocate memory for a closure and initialize it with the correct "function pointer" and environment

  syntax Instructions ::= #allocBind(String, Int, Map)       [function]
  syntax Instructions ::= #bindEnv(LocalName, Map)              [function]
  rule #allocBind(N, I, E) => (#stringToLocalName(N) = call @alloc(1 +Int size(E))
                               store I, #stringToLocalName(N)
                               #intToLocalName(!M) = #stringToLocalName(N)
                               #bindEnv(#intToLocalName(!M), E)
                              ):Instructions
// TODO: Handle order of multiple variables
  rule #bindEnv(N, .Map) => .Instructions
  rule #bindEnv(N, (X:Var |-> Y:Var) M:Map) => (N = add N, 1
                                         store #varToLocalName(X), N
                                         #bindEnv(N, M)
                                        ):Instructions


// Other constructs
// ----------------

// Currently, we put off implementing the details of IELE syntax:

  syntax Val ::= Instruction | Instructions | LocalName
  rule X:Var => #varToLocalName(X)

  syntax Tm ::= Operand
  rule ( builtin addInteger O1:Operand O2:Operand ):Tm
    => ((#stringToLocalName(#freshString) = add O1 , O2)
        .Instructions
       ):Instructions

endmodule
