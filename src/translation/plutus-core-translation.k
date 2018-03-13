require "../syntax.k"
require "iele-syntax.k"

module PLUTUS-CORE-TRANSLATION

  imports PLUTUS-CORE-COMMON
  imports IELE-COMMON
  
  // For some reason <function multiplicity='*' type='*'> isn't being parsed correctly
  // TODO: Figure out why we need this, but KEVM, IELE etc do not.
  
  syntax #CellName ::= "type"                                             [token]
  
  // Configuration
  // =============
  
  // The translation process converts a Plutus program initially loaded in the
  // `<plutus>` cell into a number of functions in the `<function>` cells.
  // We also generate functions for allocating memory, and dispatching functions.
  
  configuration
    <T>
      <plutus> $PGM:DecList ~> #createAlloc:K ~> #createInit:K ~> #createDispatch:K </plutus>
      <totalFunctions> 0 </totalFunctions>
      <totalReadyForDispatch> 0 </totalReadyForDispatch>
      <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
      <functions>
        <function multiplicity="*" type="Set">
          <name>   #mkGN("noname")       </name>
          <params> .VarList:VarList      </params>
          <env>    .Map                  </env>
          <k>      .K:K                  </k>
          <jumpId> -1                    </jumpId>
          <hasDispatchEntry> false:Bool  </hasDispatchEntry>
          <readyForDispatch> false:Bool  </readyForDispatch>
        </function>
      </functions>
    </T>
  
  // Top-Level Functions
  // ===================
  
  // For each `define` statement, we generate a top-level function that loads
  // the Term `T` into an operand and returns that operand.
  
  rule <plutus>(define N:Name T:Tm) DS:DecList => DS:DecList ... </plutus>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
       <functions>
       ... ( .Bag
          => <function>
               <name>   #nameToGlobalName(N:Name) </name>
               <jumpId> -1                        </jumpId>
               <k>      #concatInsList(#loadOperand(T:Tm, #mkLN("ret")),
                                       ret #useOperand(T:Tm, #mkLN("ret")) .Instructions)
               </k>
               <env>    .Map                      </env>
               <params> .VarList                  </params>
               <hasDispatchEntry> true:Bool        </hasDispatchEntry>
               <readyForDispatch> false:Bool       </readyForDispatch>
             </function>)
       </functions>
  rule <plutus> .DecList => .K:K ... </plutus>
  
  // Converting Plutus Constructs to IELE
  // ====================================
  
  // For each Plutus construct, the translation process needs to know how to convert
  // to convert it to the corresponding IELE construct and load that into an `Operand`.
  // This is done via `#loadOperand` and `#useOperand`S.
  
  syntax PreInstructions ::= #loadOperand(Tm, LocalName)
  syntax Operand ::= #useOperand(Tm, LocalName)                        [function]
  
  // Basic Constructs
  // ----------------
  
  // `Int`s and parameters are particularly simple, since they correspond directly
  // to `Operand`s. We do nothing to load them:
  
  rule #loadOperand(I:Int , LN) => .Instructions
  rule #loadOperand(V:Var , LN) => .Instructions
  
  // and use them directly:
  
  rule #useOperand(I:Int, LN) => I:Int
  rule #useOperand(V:Var, LN) => #varToLocalName(V:Var)
  
  // The builtin `addInteger` corresponds to the IELE `add` instruction:
  
  rule #loadOperand((builtin addInteger T1:Tm T2:Tm), LN)
    => #concatInsList(
        #concatInsList(#loadOperand(T1, #intToLocalName(!N1)),
                       #loadOperand(T2, #intToLocalName(!N2))),
         LN = add #useOperand(T1, #intToLocalName(!N1))
                , #useOperand(T2, #intToLocalName(!N2)) .Instructions)
  rule #useOperand((builtin addInteger T1:Tm T2:Tm), LN) => LN
  
  // Converting a Lambda to a Closure
  // --------------------------------
  
  // Converting a lambda is particularly complex. We must:
  //
  //  - Generate a new function for the inner part of the lambda,
  //    Its `<jumpId>` is to be used as a pseudo function pointer.
  //  - Mark variables in the body of the lambda that take their values from
  //    the parameters as `#freeVar`s, so they can correctly access their value
  //    from the closure passed.
  //  - Allocate a closure and bind its environment.
  
  rule <functions>
         ...
          <function>
            <k> #loadOperand((lam X:Var T:Tm), LN:LocalName)
             => #allocBind(LN, !N, #addParamsToEnv(VS:VarList, M:Map))
             ... </k>
            <env> M </env>
            <params> VS:VarList </params>
            ...
          </function>
          (  .Bag
          => <function>
               <name>   #intToGlobalName(!N)  </name>
               <jumpId> !N                    </jumpId>
               <k>      #concatInsList(#loadOperand(#replaceFreeVarsForParams(T, VS, size(#addParamsToEnv(VS:VarList, M:Map)) -Int 1), #mkLN("ret")),
                                       ret #useOperand(#replaceFreeVarsForParams(T, VS, size(#addParamsToEnv(VS:VarList, M:Map)) -Int 1), #mkLN("ret")) .Instructions)
               </k>
               <env>    #addParamsToEnv(VS:VarList, M:Map) </env>
               <params> X </params>
               <hasDispatchEntry> false:Bool </hasDispatchEntry>
               <readyForDispatch> false:Bool </readyForDispatch>
             </function>
          )
       </functions>
       <dispatchedFunctionsLeft> DFL => DFL +Int 1 </dispatchedFunctionsLeft>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
  
  rule #useOperand((lam X:Var T:Tm), LN:LocalName) => LN:LocalName
  
  // `#allocBind` generates instructions to allocate a closure and bind its
  // environment.
  
  syntax Instructions ::= #allocBind(LocalName, Int, Map)              [function]
  syntax Instructions ::= #bindEnv(Map, LocalName)                     [function]
  rule #allocBind(N, I, E) => (N = call #mkGN("alloc")(1 +Int size(E))
                               store I, N:LocalName
                               #bindEnv(E, N)
                              ):Instructions
  
  rule #bindEnv(.Map, _) => .Instructions
  rule #bindEnv((OFF:Int |-> V:Var) M:Map, LN) => (
                                          #intToLocalName(!N) = add LN, (OFF +Int 1)
                                          store #varToLocalName(V), #intToLocalName(!N)
                                          #bindEnv(M, LN)
                                         ):Instructions
  
  // `#addParamsToEnv` is a helper for building the environment map:
  // TODO: Should this really be called an environment?
  
  syntax Map ::= #addParamsToEnv(VarList, Map)                         [function]
  rule #addParamsToEnv(X:Var   , MAP:Map) => (size(MAP) |-> X:Var):Map MAP:Map
  rule #addParamsToEnv(.VarList, MAP:Map) => MAP:Map
  
  // TODO: Can this be folded into `#replaceFreeVar`?
  
  syntax Tm ::= #replaceFreeVarsForParams(Tm, VarList, Int)            [function]
  rule #replaceFreeVarsForParams(T:Tm, X:Var   , OFFSET) => #replaceFreeVar(T:Tm, X:Var, OFFSET)
  rule #replaceFreeVarsForParams(T:Tm, .VarList, OFFSET) => T:Tm
  
  // `#replaceFreeVar` and `#replaceFreeVarsForParams` are used to mark parameters
  // as `#freeVar`s. A rule is required for each Plutus construct.
  
  syntax Tm ::= #freeVar(Var, Int)
  syntax Tm ::= #replaceFreeVar(Tm, Var, Int)                          [function]
  
  rule #replaceFreeVar(X:Var, X    , OFFSET) => #freeVar(X, OFFSET)
  rule #replaceFreeVar(X:Var, Y:Var, OFFSET) => X:Var               when (X:Var =/=K Y:Var)
  rule #replaceFreeVar(I:Int, X    , OFFSET) => I:Int
  rule #replaceFreeVar(#freeVar(Y, J), X    , OFFSET) => #freeVar(Y:Var, J:Int)
  rule #replaceFreeVar((lam X:Var T), X:Var, OFFSET) => ((lam X:Var T)):Tm
  rule #replaceFreeVar((lam X:Var T), Y:Var, OFFSET) => ((lam X:Var #replaceFreeVar(T, Y, OFFSET))):Tm
    when (X:Var =/=K Y:Var)
  rule #replaceFreeVar([T1 T2], X:Var, OFFSET)
    => [#replaceFreeVar(T1, X, OFFSET) #replaceFreeVar(T2, X, OFFSET)]
  rule #replaceFreeVar((builtin N T1 T2), Y:Var, OFFSET)
    => (builtin N #replaceFreeVar(T1, Y, OFFSET) #replaceFreeVar(T2, Y, OFFSET))
  
  // `#freeVar`'s `#loadOperand` gets its value from the `#mkLN("closure")` allocated via `#allocBind`:
  
   rule #loadOperand(#freeVar(V, OFFSET), LN) => (#mkLN("addr") = add #mkLN("closure"), (OFFSET +Int 1)
                                                  LN    = load #mkLN("addr")
                                                 ):Instructions
   rule #useOperand(#freeVar(V, OFFSET), LN) => LN:LocalName
  
  // Lambda Applications
  // -------------------
  
  // Lambda applications simply pass the closure generated by `#allocBind` to
  // `#mkGN("dispatch")`.
  
   rule #loadOperand([T1:Tm T2:Tm], LN)
     => #concatInsList(
          #concatInsList( #loadOperand(T1, #intToLocalName(!N))
                        , #loadOperand(T2, #intToLocalName(!M)))
           , LN = call #mkGN("dispatch")( #useOperand(T1, #intToLocalName(!N))
                                , #useOperand(T2, #intToLocalName(!M)))
                        .Instructions
                      )
  rule #useOperand([T1 T2], LN:LocalName) => LN:LocalName
  
  syntax K ::= "#createAlloc" | "#createInit" | "#createDispatch"
  
  // Generate static functions
  // -------------------------
  
  rule <plutus> #createAlloc:K => .K:K ... </plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   #mkGN("alloc")       </name>
                           <jumpId> -1                   </jumpId>
                           <k>      #mkLN("alloc_counter_ptr") = 0
                                    #mkLN("alloced") = load #mkLN("alloc_counter_ptr")
                                    #mkLN("ret") = #mkLN("alloced")
                                    #mkLN("alloced") = add #mkLN("size"), #mkLN("alloced")
                                    store #mkLN("alloced"), #mkLN("alloc_counter_ptr")
                                    ret #mkLN("ret")
                           </k>
                           <env>    .Map                  </env>
                           <params> #mkVar("size") .VarList </params>
                           <hasDispatchEntry> true:Bool   </hasDispatchEntry>
                           <readyForDispatch> false:Bool  </readyForDispatch>
                         </function>
                       )
       </functions>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
  
  rule <plutus> #createInit:K => .K:K ... </plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   #mkGN("init")        </name>
                           <jumpId> -1                   </jumpId>
                           <k>      #mkLN("alloc_counter_ptr") = 0
                                    store  1, #mkLN("alloc_counter_ptr")
                           </k>
                           <env>    .Map                  </env>
                           <params> .VarList              </params>
                           <hasDispatchEntry> true:Bool   </hasDispatchEntry>
                           <readyForDispatch> false:Bool  </readyForDispatch>
                         </function>
                       )
       </functions>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
  
  syntax PreInstructions ::= "#dispatchJumpTable"
                           | #dispatchCheckCond(Int, GlobalName)
                           | #dispatchCall     (Int, GlobalName)
  rule <plutus> #createDispatch:K => .K:K ... </plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   (#mkGN("dispatch")):GlobalName </name>
                           <jumpId> -1                   </jumpId>
                           <k>
                                #concatInsList( #mkLN("fptr") = load #mkLN("closure") .Instructions
                                              , #dispatchJumpTable )
  
                           </k>
                           <env>    .Map                   </env>
                           <params> #mkVar("closure") #mkVar("param") .VarList </params>
                           <hasDispatchEntry> true:Bool        </hasDispatchEntry>
                           <readyForDispatch> false:Bool        </readyForDispatch>
                         </function>
                       )
       </functions>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
  
  rule <function>
          <name>   FN     </name>
          <jumpId> JI     </jumpId>
          <hasDispatchEntry> false:Bool => true:Bool </hasDispatchEntry>
          <readyForDispatch> true:Bool               </readyForDispatch>
          ...
       </function>
       <function>
         <k> #dispatchJumpTable:PreInstructions
                => #concatInsList( #dispatchCheckCond(JI, FN)
                                 , #concatInsList( #dispatchJumpTable
                                                 , #dispatchCall(JI, FN)))
            ...
         </k>
         ...
       </function>
       <dispatchedFunctionsLeft> DFL => DFL -Int 1 </dispatchedFunctionsLeft>
  
  rule <function>
          <k> _:Instructions </k>
          <hasDispatchEntry> _:Bool                  </hasDispatchEntry>
          <readyForDispatch> false:Bool => true:Bool </readyForDispatch>
          ...
       </function>
       <totalReadyForDispatch> TR => TR +Int 1 </totalReadyForDispatch>
  
  // Once all functions execpt #mkGN("dispatch") have been processed, ther are no more entrys to be dispatched
    rule <function>
            <k> #dispatchJumpTable => .Instructions ... </k>
            ...
         </function>
         <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
         <totalReadyForDispatch> TRFD:Int </totalReadyForDispatch>
         <totalFunctions> TF:Int </totalFunctions>
      when TF:Int ==K (TRFD:Int +Int 1)
  
    rule #dispatchCall(JI, GN)
      => #block(#mkIN("label" +String Int2String(JI:Int)))
         #mkLN("ret") = call GN(#mkLN("closure"), #mkLN("param"))
         ret #mkLN("ret")
    rule #dispatchCheckCond(JI:Int, GN:GlobalName)
      => #mkLN("cond") = cmp eq #mkLN("fptr"), JI:Int
         br #mkLN("cond"), #mkIN("label" +String Int2String(JI:Int))
  
  // Helpers for ...
  // ===============
  
  // ... generating `Instruction`s
  // ------------------------------
  
   syntax PreInstructions ::= Instructions
   syntax KResult         ::= Instructions
  
  syntax PreInstructions ::= #concatInsList(PreInstructions, PreInstructions)
                                                                         [strict]
  rule #concatInsList(IS:Instructions, .Instructions) => IS:Instructions
  rule #concatInsList(IS:Instructions, I IS')         => #concatInsList(#addInsToBack(IS:Instructions, I:Instruction .Instructions), IS')
  
  syntax Instructions ::= #addInsToBack(Instructions, Instructions)    [function]
  rule #addInsToBack(.Instructions, IS) => IS:Instructions
  rule #addInsToBack(I IS, IS') => I #addInsToBack(IS, IS')
  
  // TODO: We should be using `Block`s instead of `Instructions` in some places
  // TODO: Rename to `#label`
  
  syntax Instruction ::= #block(IeleName)

  // Converting strings to {Iele,Local,Global}Name and Vars
  // ------------------------------------------------------

  syntax GlobalName ::= #mkGN(String)                                [function]
  syntax LocalName  ::= #mkLN(String)                                [function]
  syntax IeleName   ::= #mkIN(String)                                [function]
  syntax Var        ::= #mkVar(String)                               [function]
  // --------------------------------------------------------------------------
  rule #mkGN(S)   => #parseToken("GlobalName", "@" +String S)
  rule #mkLN(S)   => #parseToken("LocalName",  "%" +String S)
  rule #mkIN(S)   => #parseToken("IeleName",               S)
  rule #mkVar(S)  => #parseToken("Var",                    S)
 
  syntax GlobalName ::= #nameToGlobalName(Name)                      [function]
  rule #nameToGlobalName(N:Name) => #mkGN(#nameToString(N:Name))
 
  syntax LocalName ::= #varToLocalName(Var)                          [function]
  rule #varToLocalName(V:Var) => #mkLN(#varToString(V))

  syntax String ::= #nameToString(Name)   [function, hook(STRING.token2string)]
  syntax String ::= #varToString(Var)     [function, hook(STRING.token2string)]

  // Helpers for fresh variables
  // ---------------------------

  syntax GlobalName ::= #intToGlobalName(Int)                        [function]
  syntax LocalName  ::= #intToLocalName(Int)                         [function]
  // --------------------------------------------------------------------------
  rule #intToLocalName(N)      => #mkLN("fresh" +String Int2String(N))
  rule #intToGlobalName(N:Int) => #mkGN("fresh" +String Int2String(N:Int))
 

endmodule
