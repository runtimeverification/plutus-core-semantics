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
          <name>   (@noname):GlobalName  </name>
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
               <k>      #concatInsList(#loadOperand(T:Tm, %ret),
                                       ret #useOperand(T:Tm, %ret) .Instructions)
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
               <k>      #concatInsList(#loadOperand(#replaceFreeVarsForParams(T, VS, size(#addParamsToEnv(VS:VarList, M:Map)) -Int 1), %ret),
                                       ret #useOperand(#replaceFreeVarsForParams(T, VS, size(#addParamsToEnv(VS:VarList, M:Map)) -Int 1), %ret) .Instructions)
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
  rule #allocBind(N, I, E) => (N = call @alloc(1 +Int size(E))
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
  
  // `#freeVar`'s `#loadOperand` gets its value from the `%closure` allocated via `#allocBind`:
  
   rule #loadOperand(#freeVar(V, OFFSET), LN) => (%addr = add %closure, (OFFSET +Int 1)
                                                  LN    = load %addr
                                                 ):Instructions
   rule #useOperand(#freeVar(V, OFFSET), LN) => LN:LocalName
  
  // Lambda Applications
  // -------------------
  
  // Lambda applications simply pass the closure generated by `#allocBind` to
  // `@dispatch`.
  
   rule #loadOperand([T1:Tm T2:Tm], LN)
     => #concatInsList(
          #concatInsList( #loadOperand(T1, #intToLocalName(!N))
                        , #loadOperand(T2, #intToLocalName(!M)))
           , LN = call @dispatch( #useOperand(T1, #intToLocalName(!N))
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
                           <name>   (@alloc):GlobalName </name>
                           <jumpId> -1                   </jumpId>
                           <k>      %alloc_counter_ptr = 0
                                    %alloced = load %alloc_counter_ptr
                                    %ret = %alloced
                                    %alloced = add %size, %alloced
                                    store  %alloced, %alloc_counter_ptr
                                    ret %ret
                           </k>
                           <env>    .Map                  </env>
                           <params> size .VarList         </params>
                           <hasDispatchEntry> true:Bool        </hasDispatchEntry>
                           <readyForDispatch> false:Bool   </readyForDispatch>
                         </function>
                       )
       </functions>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
  
  rule <plutus> #createInit:K => .K:K ... </plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   (@init):GlobalName </name>
                           <jumpId> -1                   </jumpId>
                           <k>      %alloc_counter_ptr = 0
                                    store  1, %alloc_counter_ptr
                           </k>
                           <env>    .Map                  </env>
                           <params> .VarList         </params>
                           <hasDispatchEntry> true:Bool        </hasDispatchEntry>
                           <readyForDispatch> false:Bool        </readyForDispatch>
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
                           <name>   (@dispatch):GlobalName </name>
                           <jumpId> -1                   </jumpId>
                           <k>
                                #concatInsList( %fptr = load %closure .Instructions
                                              , #dispatchJumpTable )
  
                           </k>
                           <env>    .Map                   </env>
                           <params> closure param .VarList </params>
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
  
  // Once all functions execpt @dispatch have been processed, ther are no more entrys to be dispatched
    rule <function>
            <k> #dispatchJumpTable => .Instructions ... </k>
            ...
         </function>
         <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
         <totalReadyForDispatch> TRFD:Int </totalReadyForDispatch>
         <totalFunctions> TF:Int </totalFunctions>
      when TF:Int ==K (TRFD:Int +Int 1)
  
    rule #dispatchCall(JI, GN)
      => #block(#stringToIeleName("label" +String Int2String(JI:Int)))
         %ret = call GN(%closure, %param)
         ret %ret
    rule #dispatchCheckCond(JI:Int, GN:GlobalName)
      => %cond = cmp eq %fptr, JI:Int
         br %cond, #stringToIeleName("label" +String Int2String(JI:Int))
  
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
  
  
  // ... generating Fresh IDs, and converting between different types of IDs
  // -----------------------------------------------------------------------
  
  syntax GlobalName ::= #stringToGlobalName(String)                    [function]
  rule #stringToGlobalName(S) => #parseToken("GlobalName", "@" +String S)
  
  syntax GlobalName ::= #intToGlobalName(Int)                          [function]
  rule #intToGlobalName(N:Int)
    => #stringToGlobalName("fresh" +String Int2String(N:Int))
  
  syntax String     ::= #nameToString(Name) [function, hook(STRING.token2string)]
  syntax String       ::= #varToString(Var) [function, hook(STRING.token2string)]
  
  syntax GlobalName ::= #nameToGlobalName(Name)                        [function]
  rule #nameToGlobalName(N:Name) => #stringToGlobalName(#nameToString(N:Name))
  
  syntax LocalName    ::= #varToLocalName(Var)                         [function]
  rule #varToLocalName(V:Var) => #stringToLocalName(#varToString(V))
  
  syntax LocalName ::= #stringToLocalName(String)                      [function]
  rule #stringToLocalName(S) => #parseToken("LocalName", "%" +String S)
  
  syntax LocalName ::= #intToLocalName(Int)                            [function]
  rule #intToLocalName(N) => #stringToLocalName("fresh" +String Int2String(N))
  
  syntax IeleName ::= #stringToIeleName(String)                        [function]
  rule #stringToIeleName(S) => #parseToken("IeleName", S)
  
  // Syntax Helpers
  // --------------
  
  // Since we import IELE-COMMON and PLUTUS-CORE-COMMON instead of the SYNTAX
  // modules, K does not recognize the various names we use in the translation.
  // unless we explicitly tell it so. TODO: Should we use `#parseToken` instead?
  
  syntax Var      ::= "closure"  | "param" | "size"
  syntax IeleName ::= "addr"
                    | "param" | "dispatch"
                    | "cond" | "fptr" | "noname"
                    | "alloc" | "alloc_counter_ptr" | "alloced"
  syntax LocalName ::= "%size" | "%closure" | "%param" | "%ret"
  
endmodule
