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

  syntax Tm  ::= #closure(String, Var, Tm, Map)                         [strict(3)]
               | #replaceFreeVar(Tm, Var, Int)                          [function]
  syntax Val ::= #freeVar(Var, Int)

// Top-level lambdas uses the names that the user has given them...
  rule (define N:Name (lam X:Var T:Tm))
    => #closure(#freshString, X, T, .Map)

// ... whereas nested lambdas are assigned a fresh name
  rule #closure(N, X:Var, #freeLambda(Y, T), M)
    => #closure(N, X, #closure( #freshString, Y
                              , #replaceFreeVar(T, X, size(M))
                              , (size(M) |-> X:Var) M)
                              , M )

// Mark free variables so that we can look up their values in the env map.
  rule #replaceFreeVar(X:Var, X    , OFFSET) => (#freeVar(X, OFFSET)):Val
  rule #replaceFreeVar(X:Var, Y:Var, OFFSET) => X:Var             when (X:Var =/=K Y:Var)
  rule #replaceFreeVar((builtin N T1 T2), Y:Var, OFFSET)
    => (builtin N #replaceFreeVar(T1, Y, OFFSET) #replaceFreeVar(T2, Y, OFFSET))

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
  syntax Instructions ::= #bindEnv(Map)                      [function]
  rule #allocBind(N, I, E) => (#stringToLocalName(N) = call @alloc(1 +Int size(E))
                               store I, #stringToLocalName(N)
                               #bindEnv(E)
                              ):Instructions
// TODO: Handle order of multiple variables
  rule #bindEnv(.Map) => .Instructions
  rule #bindEnv((OFF:Int |-> V:Var) M:Map) => (
                                         #intToLocalName(!N) = add #stringToLocalName("env"), (OFF +Int 1)
                                         store #varToLocalName(V), #intToLocalName(!N)
                                         #bindEnv(M)
                                        ):Instructions


// Other constructs
// ----------------

// Currently, we put off implementing the details of IELE syntax:

  syntax Val ::= Instruction | Instructions | LocalName
  rule X:Var => #varToLocalName(X)

  syntax Instructions ::= #loadOperand(Tm, LocalName)               [function]
  rule #loadOperand(I:Int              , LN) => .Instructions
  rule #loadOperand(V:Var              , LN) => .Instructions
  rule #loadOperand(N:LocalName        , LN) => .Instructions
  rule #loadOperand(#freeVar(V, OFFSET), LN) => (%addr = add #stringToLocalName("env"), OFFSET
                                                 LN    = load %addr
                                                ):Instructions

  syntax Operand ::= #useOperand(Tm, LocalName)                     [function]
  rule #useOperand(I:Int              , LN) => I:Int
  rule #useOperand(V:Var              , LN) => #varToLocalName(V:Var)
  rule #useOperand(N:LocalName        , LN) => N:LocalName
  rule #useOperand(#freeVar(V, OFFSET), LN) => LN:LocalName

  syntax Instructions ::= #concatInsList(Instructions, Instructions) [function]
  rule #concatInsList(.Instructions, IS) => IS:Instructions
  rule #concatInsList(I IS, IS')         => #concatInsList(IS, I IS')

  rule (builtin addInteger T1:Tm T2:Tm):Tm
    => #concatInsList(
         #concatInsList(#loadOperand(T1, #intToLocalName(!N1)),
                        #loadOperand(T2, #intToLocalName(!N2))),
         #stringToLocalName(#freshString)
                = add #useOperand(T1, #intToLocalName(!N1))
                    , #useOperand(T2, #intToLocalName(!N2)))

endmodule
