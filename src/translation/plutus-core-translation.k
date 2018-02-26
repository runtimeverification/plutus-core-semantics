require "../syntax.k"
require "iele-syntax.k"

module PLUTUS-CORE-TRANSLATION
  imports PLUTUS-CORE-SYNTAX

// Pull off some stunts to avoid ambiguities between `.K` and IELEName
  imports IELE-COMMON
  syntax IeleName ::= r"(?<![A-Za-z0-9\\_\\-\\$])[a-zA-Z\\_\\-\\$][0-9a-zA-Z\\_\\-\\$]*" [token, notInRules, prec(3)]

  configuration
    <T>
      <plutus> $PGM:DecList </plutus>
      <functions>
        <function multiplicity="*" type="Bag">
          <name>   (@noname):GlobalName </name>
          <jumpId> -1                   </jumpId>
          <k>      .K                    </k>
          <env>    .Map                  </env>
          <params> .LocalNames          </params>
        </function>
      </functions>
    </T>

// We translate each declaration iteratively
  rule <plutus>(define N:Name T:Tm) DS:DecList => DS</plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   #nameToGlobalName(N:Name) </name>
                           <jumpId> -1                        </jumpId>
                           <k>      #concatInsList(#loadOperand(T:Tm, #stringToLocalName("ret")),
                                                   ret #useOperand(T:Tm, #stringToLocalName("ret")) .Instructions)
                           </k>
                           <env>    .Map                      </env>
                           <params> .LocalNames               </params>
                         </function>
                       )
       </functions>

  rule <functions>
           <function>
             <k> #loadOperand((lam X:Var T:Tm), LN:LocalName)
              => #allocBind(#freshString, !N, ((size(M)):Int |-> X:Var):Map M:Map)
              ... </k>
             <env> M </env>
             <name> N:GlobalName </name>
             <params> LNS:LocalNames </params>
             <jumpId> J:Int </jumpId>
           </function>
           (  .Bag
           => <function>
                <name>   #intToGlobalName(!N)  </name>
                <jumpId> !N                    </jumpId>
                <k>      #loadOperand(#replaceFreeVar(T:Tm, X:Var, size(M)), #stringToLocalName("ret"))
                </k>
                <env>    ((size(M)):Int |-> X:Var):Map M:Map </env>
                <params> #varToLocalName(X)    </params>
              </function>
           )
          ...
        </functions>

  rule #useOperand((lam X:Var T:Tm), LN:LocalName) => LN:LocalName
// Helpers
// -------

  syntax GlobalName ::= #nameToGlobalName(Name)                           [function]

   syntax LocalName ::= #stringToLocalName(String)                   [function]
   rule #stringToLocalName(S) => #parseToken("LocalName@IELE-COMMON", "%" +String S)

   syntax GlobalName ::= #stringToGlobalName(String)                   [function]
   rule #stringToGlobalName(S) => #parseToken("GlobalName@IELE-COMMON", "@" +String S)
   syntax GlobalName ::= #intToGlobalName(Int)       [function]
   rule #intToGlobalName(N:Int)
     => #stringToGlobalName("fresh" +String Int2String(N:Int))

   syntax String       ::= #varToString(Var)       [function, hook(STRING.token2string)]
   syntax LocalName    ::= #varToLocalName(Var)    [function]
   rule #varToLocalName(V:Var) => #stringToLocalName(#varToString(V))

  syntax LocalName ::= #intToLocalName(Int)       [function]
  rule #intToLocalName(N)
    => #stringToLocalName("fresh" +String Int2String(N))
  syntax String ::= "#freshString"                            [function]
  rule #freshString:String
    => "fresh" +String Int2String(!N)

// // Convert Lambdas
// // ---------------

//   syntax Tm  ::= #closure(String, Var, Tm, Map)                         [strict(3)]

// // Top-level lambdas uses the names that the user has given them...
//   rule (define N:Name (lam X:Var T:Tm))
//     => #closure(#freshString, X, T, .Map)

// // ... whereas nested lambdas are assigned a fresh name
//   rule #closure(N, X:Var, #freeLambda(Y, T), M)
//     => #closure(N, X, #closure( #freshString, Y
//                               , #replaceFreeVar(T, X, size(M))
//                               , (size(M) |-> X:Var) M)
//                               , M )

// Mark free variables so that we can look up their values in the env map.
  syntax Tm ::= #replaceFreeVar(Tm, Var, Int)                          [function]
  syntax Tm ::= #freeVar(Var, Int)
  rule #replaceFreeVar(X:Var, X    , OFFSET) => #freeVar(X, OFFSET)
  rule #replaceFreeVar(X:Var, Y:Var, OFFSET) => X:Var             when (X:Var =/=K Y:Var)
  rule #replaceFreeVar(I:Int, X    , OFFSET) => I:Int
  rule #replaceFreeVar((lam X:Var T), Y:Var, OFFSET)
    => ((lam X:Var T)):Tm
  rule #replaceFreeVar((builtin N T1 T2), Y:Var, OFFSET)
    => (builtin N #replaceFreeVar(T1, Y, OFFSET) #replaceFreeVar(T2, Y, OFFSET))

//   syntax Val ::= #freeLambda(Var, Tm)
//   rule (lam X:Var T:Tm):Tm
//     => #freeLambda(X, T)

// // These are then converted to IELEFunctions (though we just abstract this for now.)

//   syntax FunctionDefinition ::= #toIELEFunction(String, FunctionParameters, Instructions) [function]
//   rule #toIELEFunction(N, PARAMS, I)
//     => define #stringToGlobalName(N) (PARAMS) { I .LabeledBlocks }

//   // TODO: E needs to access env
//   rule <k> #closure(NAME:String, X, I:Instructions, M) =>
//            #allocBind(NAME, !N, M)
//         ... </k>
//        <functions> ...
//              .Map
//          => (!N:Int |-> #toIELEFunction(NAME, (#stringToLocalName("env"), #varToLocalName(X)), I))
//        </functions>



// `#allocBind` will allocate memory for a closure and initialize it with the correct "function pointer" and environment

  syntax Instructions ::= #allocBind(String, Int, Map)       [function]
  syntax Instructions ::= #bindEnv(Map, LocalName)                      [function]
  rule #allocBind(N, I, E) => (#stringToLocalName(N) = call @alloc(1 +Int size(E))
                               store I, #stringToLocalName(N)
                               #bindEnv(E, #stringToLocalName(N))
                              ):Instructions

  rule #bindEnv(.Map, _) => .Instructions
  rule #bindEnv((OFF:Int |-> V:Var) M:Map, LN) => (
                                          #intToLocalName(!N) = add LN, (OFF +Int 1)
                                          store #varToLocalName(V), #intToLocalName(!N)
                                          #bindEnv(M, LN)
                                         ):Instructions

// Other constructs
// ----------------

   syntax PreInstructions ::= Instructions
   syntax KResult         ::= Instructions

   syntax PreInstructions ::= #concatInsList(PreInstructions, PreInstructions) [strict]
   rule #concatInsList(IS, .Instructions) => IS:Instructions
   rule #concatInsList(IS, I IS')         => #concatInsList(#addInsToBack(IS, I .Instructions), IS')

   syntax Instructions ::= #addInsToBack(Instructions, Instructions) [function]
   rule #addInsToBack(.Instructions, IS) => IS
   rule #addInsToBack(I IS, IS') => I #addInsToBack(IS, IS')

   syntax PreInstructions ::= #loadOperand(Tm, LocalName)
   rule #loadOperand(I:Int              , LN) => .Instructions
   rule #loadOperand(V:Var              , LN) => .Instructions
   rule #loadOperand(#freeVar(V, OFFSET), LN) => (%addr = add #stringToLocalName("env"), OFFSET
                                                  LN    = load %addr
                                                 ):Instructions
   rule #loadOperand((builtin addInteger T1:Tm T2:Tm), LN)
     => #concatInsList(
         #concatInsList(#loadOperand(T1, #intToLocalName(!N1)),
                        #loadOperand(T2, #intToLocalName(!N2))),
          LN = add #useOperand(T1, #intToLocalName(!N1))
                 , #useOperand(T2, #intToLocalName(!N2)) .Instructions)

   syntax Operand ::= #useOperand(Tm, LocalName)                     [function]
   rule #useOperand(I:Int              , LN) => I:Int
   rule #useOperand(V:Var              , LN) => #varToLocalName(V:Var)
   rule #useOperand(#freeVar(V, OFFSET), LN) => LN:LocalName

endmodule
