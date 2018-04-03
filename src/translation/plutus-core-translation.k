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
      <plutus> $PGM:Prg ~> #createDispatch:K ~> #moveToContractCell </plutus>
      <iele> #staticAlloc
             #staticAppendArg
             .TopLevelDefinitions
      </iele>
      <ieleContract> contract #mkIN("Foo") { .TopLevelDefinitions } </ieleContract>
      <totalFunctions> 0 </totalFunctions>
      <totalReadyForDispatch> 0 </totalReadyForDispatch>
      <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
      <exports> .NameList:NameList </exports>
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


  rule <plutus> (program (module MN IS (export (TXS) (NS)) DS) ) => DS ... </plutus>
       <exports> _ => NS </exports>

  // We iterate over `Dec`s, creating a named top level function for each.

  rule <plutus> D:Dec DS:DecList => D ~> DS ... </plutus>

  rule <plutus> (define N:Name V:Val)
             => #topLevelFunction(#nameToGlobalName(N), !JI, .VarList, V)
                ...
       </plutus>
  rule <plutus> .DecList => .K ... </plutus>

  // Ignore `Dec`s related to typing

  rule <plutus> _:TDec => .K ... </plutus>
  rule <plutus> _:MDec => .K ... </plutus>

  // TODO: DDec

  syntax K ::= #topLevelFunction(GlobalName, Int, VarList, Tm)
  rule <plutus> #topLevelFunction(GN, JI, VARS:VarList, (lam X:Var T:Tm))
             => #topLevelFunction(GN, JI, X VARS, #replaceFreeVar(T:Tm, X, #lengthVarList(VARS)))
                ...
       </plutus>
  rule <plutus> #topLevelFunction(GN, JI, VARS:VarList, T:Tm) => .K ... </plutus>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
       <dispatchedFunctionsLeft> DFL => DFL +Int 1 </dispatchedFunctionsLeft>
       <functions> ...
         .Bag =>
         <function>
            <name> GN </name>
            <params> #reverseVarList(VARS) </params>
            <jumpId> JI </jumpId>
            <k>      #concatInsList(#loadOperand(T:Tm, #mkLN("ret")),
                                    ret #useOperand(T:Tm, #mkLN("ret")) .Instructions)
            </k>
            <env> .Map </env>
            <hasDispatchEntry> false:Bool </hasDispatchEntry>
            <readyForDispatch> false:Bool </readyForDispatch>
         </function>
       </functions>                                                       [owise]
  
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
  
  rule #loadOperand(I:Int, LN) => .Instructions
  rule #loadOperand(V:Var, LN) => .Instructions
  
  // and use them directly:
  
  rule #useOperand(I:Int, LN) => I:Int
  rule #useOperand(V:Var, LN) => #varToLocalName(V:Var)

  // Qualified Names
  // ---------------

  rule #useOperand(QN:QualN, LN) => LN
  // TODO: test this using IELE semantics
  rule <functions>
         ...
          <function>
            <name> GN </name>
            <jumpId> JI </jumpId>
            ...
          </function>
          <function>
            <k> #loadOperand(MN:ModName . N:Name, LN:LocalName)
             => #allocBind(LN, JI, .Map)
             ... </k>
            ...
          </function>
       </functions>
       when #nameToGlobalName(N) ==K GN

  // Builtins
  // --------

  // Binary builtins share code for `#loadOperand` and `#useOperand`.

  rule #useOperand((builtin B:BinaryBuiltin  T1:Tm T2:Tm), LN) => LN
  rule #loadOperand((builtin B:BinaryBuiltin T1:Tm T2:Tm), LN)
    => #concatInsList(
       #concatInsList(#loadOperand(T1, #intToLocalName(!N1)),
                      #loadOperand(T2, #intToLocalName(!N2))),
                      #translateBinBuiltin(B:BinaryBuiltin, LN
                            , #useOperand(T1, #intToLocalName(!N1))
                            , #useOperand(T2, #intToLocalName(!N2))))

  // Each needs to generate instructions for executing in IELE.

  syntax Instructions ::= #translateBinBuiltin(BinaryBuiltin, LocalName, Operand, Operand) [function]
  // ------------------------------------------------------------------------------------------------
  rule #translateBinBuiltin(addInteger,       LN, O1, O2) => (LN = add O1 , O2) .Instructions
  rule #translateBinBuiltin(subtractInteger,  LN, O1, O2) => (LN = sub O1 , O2) .Instructions
  rule #translateBinBuiltin(multiplyInteger,  LN, O1, O2) => (LN = mul O1 , O2) .Instructions
  rule #translateBinBuiltin(divideInteger,    LN, O1, O2) => (LN = div O1 , O2) .Instructions
  rule #translateBinBuiltin(remainderInteger, LN, O1, O2) => (LN = mod O1 , O2) .Instructions

  rule #translateBinBuiltin(lessThanInteger,          LN, O1, O2) => (LN = cmp lt O1, O2) .Instructions
  rule #translateBinBuiltin(lessThanEqualsInteger,    LN, O1, O2) => (LN = cmp le O1, O2) .Instructions
  rule #translateBinBuiltin(greaterThanInteger,       LN, O1, O2) => (LN = cmp gt O1, O2) .Instructions
  rule #translateBinBuiltin(greaterThanEqualsInteger, LN, O1, O2) => (LN = cmp ge O1, O2) .Instructions
  rule #translateBinBuiltin(equalsInteger,            LN, O1, O2) => (LN = cmp eq O1, O2) .Instructions

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
       </functions>
       <plutus> (.K => #topLevelFunction(#intToGlobalName(!N), !N, X, T:Tm)) ... </plutus>

  rule #useOperand((lam X:Var T:Tm), LN:LocalName) => LN:LocalName
  
  // `#allocBind` generates instructions to allocate a closure and bind its
  // environment.
  
  syntax Instructions ::= #allocBind(LocalName, Int, Map)              [function]
  syntax Instructions ::= #bindEnv(Map, LocalName)                     [function]

  rule #allocBind(N, JI, E) => (N = call #mkGN("alloc")(1 +Int size(E))
                                store JI, N:LocalName, 0, 1
                                store 0, N:LocalName,  1, 1
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
  rule #addParamsToEnv(X:Var XS, MAP:Map) => #addParamsToEnv(XS, (size(MAP) |-> X:Var):Map MAP:Map)
  rule #addParamsToEnv(.VarList, MAP:Map) => MAP:Map
  
  // TODO: Can this be folded into `#replaceFreeVar`?
  
  syntax Tm ::= #replaceFreeVarsForParams(Tm, VarList, Int)            [function]
  rule #replaceFreeVarsForParams(T:Tm, X:Var   , PARAMINDEX) => #replaceFreeVar(T:Tm, X:Var, PARAMINDEX)
  rule #replaceFreeVarsForParams(T:Tm, .VarList, PARAMINDEX) => T:Tm
  
  // `#replaceFreeVar` and `#replaceFreeVarsForParams` are used to mark parameters
  // as `#freeVar`s. A rule is required for each Plutus construct.
  
  syntax Tm ::= #freeVar(Var, Int)
  syntax Tm ::= #replaceFreeVar(Tm, Var, Int)                          [function]
  
  rule #replaceFreeVar(X:Var, X    , INDEX) => #freeVar(X, INDEX)
  rule #replaceFreeVar(X:Var, Y:Var, INDEX) => X:Var               when (X:Var =/=K Y:Var)
  rule #replaceFreeVar(I:Int, X    , INDEX) => I:Int
  rule #replaceFreeVar(QN:QualN, X:Var, INDEX) => QN
  rule #replaceFreeVar(#freeVar(Y, J), X    , INDEX) => #freeVar(Y:Var, J:Int)
  rule #replaceFreeVar((lam X:Var T), X:Var, INDEX) => ((lam X:Var T)):Tm
  rule #replaceFreeVar((lam X:Var T), Y:Var, INDEX) => ((lam X:Var #replaceFreeVar(T, Y, INDEX))):Tm
    when (X:Var =/=K Y:Var)
  rule #replaceFreeVar([T1 T2], X:Var, INDEX)
    => [#replaceFreeVar(T1, X, INDEX) #replaceFreeVar(T2, X, INDEX)]
  rule #replaceFreeVar((builtin N T1 T2), Y:Var, INDEX)
    => (builtin N #replaceFreeVar(T1, Y, INDEX) #replaceFreeVar(T2, Y, INDEX))
  
  // For `#freeVar`s `#loadOperand` directly uses the function's parameter.
  // TODO: Make use of INDEX to prevent collisions in case of non-unique argument names.
  rule #loadOperand(#freeVar(V, INDEX), LN) => .Instructions
  rule #useOperand(#freeVar(V, INDEX), LN) => #varToLocalName(V) 
  
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
  
  // Generate static functions
  // =========================

  syntax FunctionDefinition ::= "#staticAlloc"                         [function]
  rule #staticAlloc => define #mkGN("alloc") (#mkLN("size") , .LocalNames) {
                         #mkLN("alloc_counter_ptr") = 0
                         #mkLN("alloced") = load #mkLN("alloc_counter_ptr")
                         #mkLN("isInitialized") = cmp ne 0, #mkLN("alloced")
                         br #mkLN("isInitialized"), #mkIN("initialized")
                         #mkLN("alloced") = 1
                      #label(#mkIN("initialized"))
                         #mkLN("ret") = #mkLN("alloced")
                         #mkLN("alloced") = add #mkLN("size"), #mkLN("alloced")
                         store #mkLN("alloced"), #mkLN("alloc_counter_ptr")
                         ret #mkLN("ret")
                         .Instructions .LabeledBlocks
                       }
  
  // Generate the dispatch function
  // ------------------------------

  syntax K ::= "#createDispatch"

  // Building the dispatch table
  // ---------------------------
  
  syntax PreInstructions ::= "#dispatchJumpTable"
                                             /* JI , NAME      , NUMARGS */
                           | #dispatchCheckCond(Int, GlobalName, Int)
                           | #dispatchCall     (Int, GlobalName, Int)
  rule <plutus> #createDispatch:K => .K:K ... </plutus>
       <functions> ... ( .Bag
                      => <function>
                           <name>   (#mkGN("dispatch")):GlobalName </name>
                           <jumpId> -1                   </jumpId>
                           <k>
                                #concatInsList( #mkLN("closureId") = load #mkLN("closure")
                                                .Instructions
                                              , #dispatchJumpTable )
  
                           </k>
                           <env>    .Map                   </env>
                           <params> #mkVar("closure") #mkVar("param") .VarList </params>
                           <hasDispatchEntry> true:Bool         </hasDispatchEntry>
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
          <params> PARAMS                            </params>
          ...
       </function>
       <function>
         <k> #dispatchJumpTable:PreInstructions
                => #concatInsList( #dispatchCheckCond(JI, FN, #lengthVarList(PARAMS))
                                 , #concatInsList( #dispatchJumpTable
                                                 , #dispatchCall(JI, FN, #lengthVarList(PARAMS))))
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
          <k> #dispatchJumpTable =>
              #mkLN("ret") = call #mkGN("appendArg")(#mkLN("closure"), #mkLN("closureId"), #mkLN("param"))
              ret #mkLN("ret")
              ...
         </k>
          ...
       </function>
       <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
       <totalReadyForDispatch> TRFD:Int </totalReadyForDispatch>
       <totalFunctions> TF:Int </totalFunctions>
    when TF:Int ==K (TRFD:Int +Int 1)

  syntax FunctionDefinition ::= "#staticAppendArg"                     [function]
  rule #staticAppendArg
    => define #mkGN("appendArg") (#mkLN("closure"), #mkLN("closureId"), #mkLN("param")) {
           #mkLN("numParams")   = load #mkLN("closure"),   1, 1
           #mkLN("numParams")   = add  #mkLN("numParams"), 1
           #mkLN("spaceNeeded") = add  #mkLN("numParams"), 1
           #mkLN("ret")         = call #mkGN("alloc")(#mkLN("spaceNeeded"))

           #comment("Location in returned closure to write to")
           #mkLN("writeAddr") = #mkLN("ret")

           #comment("Copy closure ID")
           store #mkLN("closureId"), #mkLN("writeAddr")

           #comment("Update number of parameters")
           store #mkLN("numParams"), #mkLN("writeAddr"), 1, 1

           #comment("Copy arguments one at a time")
           #label(#mkIN("while"))
           #mkLN("numParams") = sub #mkLN("numParams"), 1
           #mkLN("cond")      = cmp eq #mkLN("numParams"), 0
           br #mkLN("cond"), #mkIN("done")

           #mkLN("writeAddr") = add #mkLN("writeAddr"), 1
           #mkLN("closure")   = add #mkLN("closure"),   1
           #mkLN("v")         = load #mkLN("closure")
           store #mkLN("v"), #mkLN("writeAddr")

           br #mkIN("while")

        #label(#mkIN("done"))
           #comment("Add new parameter")
           #mkLN("writeAddr") = add #mkLN("writeAddr"), 1
           store #mkLN("param"), #mkLN("writeAddr")

           ret #mkLN("ret")
           .Instructions .LabeledBlocks
        }

  // The closureId is the first entry in a closure. It is two bytes long, with
  // the MSB the Jump ID, and the other the number of arguments stored in the
  // closure.
  syntax Operand ::= #closureId(Int, Int)                              [function]
  rule #closureId(JI, NUMARGS) => NUMARGS +Int (JI <<Int 8)

  // If the closure contains one less than the number of arguments that the
  // function accepts, we call the function
  rule #dispatchCheckCond(JI, GN, NUMARGS)
    => #mkLN("cond") = cmp eq #mkLN("closureId"), #closureId(JI, NUMARGS -Int 1)
       br #mkLN("cond"), #mkIN("call" +String Int2String(JI:Int))

  rule #dispatchCall(JI, GN, NUMARGS)
    => #label(#mkIN("call" +String Int2String(JI:Int)))
       #unfoldClosure(NUMARGS, 1, GN, ret #mkLN("ret"))

  syntax Instructions ::= #unfoldClosure(Int, Int, GlobalName, Instructions)
                                                                       [function]
  rule #unfoldClosure(NUMARGS, I, GN, IS)
    => #mkLN("ret") = call GN(#makeArgsList(NUMARGS -Int 1, #mkLN("param")))
       IS
    when I ==Int NUMARGS
  rule #unfoldClosure(0, _, _, _)
    => call #mkGN("iele.invalid")(.Operands) .Instructions
  rule #unfoldClosure(NUMARGS, I, GN, IS)
    => #mkLN("closure") = add #mkLN("closure"), 1
       #mkLN("param" +String Int2String(I)) = load #mkLN("closure")
       #unfoldClosure(NUMARGS, I +Int 1, GN, IS)
    when NUMARGS >Int 0 andBool I =/=Int NUMARGS

  syntax Operands ::= #makeArgsList(Int, Operands)                     [function]
  rule #makeArgsList(0, OPS) => OPS
  rule #makeArgsList(NUMARGS, OPS)
    => #makeArgsList(NUMARGS -Int 1, #mkLN("param" +String Int2String(NUMARGS)), OPS)
    when NUMARGS >Int 0

  // Finally, convert `<function>`s to IELE syntax
  rule <functions>
         <function>
           <name>   GN:GlobalName         </name>
           <params> VARS:VarList          </params>
           <env>    ENV                   </env>
           <k>      IS:Instructions       </k>
           ...
         </function> => .Bag
         ...
       </functions>
       <iele> TLDS:TopLevelDefinitions
           => (define public GN (#varsToLocalNames(VARS)) { IS .LabeledBlocks }):FunctionDefinition TLDS
       </iele>
       <exports> NL </exports>
       <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
       <totalReadyForDispatch> TF </totalReadyForDispatch>
       <totalFunctions> TF </totalFunctions>
       when #in(GN, NL)

  rule <functions>
         <function>
           <name>   GN:GlobalName         </name>
           <params> VARS:VarList          </params>
           <env>    ENV                   </env>
           <k>      IS:Instructions       </k>
           ...
         </function> => .Bag
         ...
       </functions>
       <iele> TLDS:TopLevelDefinitions
           => (define GN (#varsToLocalNames(VARS)) { IS .LabeledBlocks }):FunctionDefinition TLDS
       </iele>
       <exports> NL </exports>
       <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
       <totalReadyForDispatch> TF </totalReadyForDispatch>
       <totalFunctions> TF </totalFunctions>
       when notBool #in(GN, NL)

  // When there are no functions left, return a top-level contract

  syntax K ::= "#moveToContractCell"
  rule <functions>
         .Bag
       </functions>
       <plutus> #moveToContractCell => .K ... </plutus>
       <iele> TLDS:TopLevelDefinitions => .TopLevelDefinitions </iele>
       <ieleContract> _ => contract #mkIN("Foo") { TLDS } </ieleContract>

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

  // "Cheats" for making generating some tokens easier
  // -------------------------------------------------
  syntax Instruction ::= #label(IeleName)                         [format(%n%3:)]
  syntax Instruction ::= #comment(String)                       [format(%n// %3)]

  // Converting strings to {Iele,Local,Global}Name and Vars
  // ------------------------------------------------------

  syntax GlobalName ::= #mkGN(String)                                  [function]
  syntax LocalName  ::= #mkLN(String)                                  [function]
  syntax IeleName   ::= #mkIN(String)                                  [function]
  syntax Var        ::= #mkVar(String)                                 [function]
  // ----------------------------------------------------------------------------
  rule #mkGN(S)   => #parseToken("GlobalName", "@" +String S)
  rule #mkLN(S)   => #parseToken("LocalName",  "%" +String S)
  rule #mkIN(S)   => #parseToken("IeleName",               S)
  rule #mkVar(S)  => #parseToken("Var",                    S)

  // Converting Names, Vars to GlobalNames, LocalNames, Strings
  // ----------------------------------------------------------

  syntax GlobalName ::= #nameToGlobalName(Name)                        [function]
  rule #nameToGlobalName(N:Name) => #mkGN(#nameToString(N:Name))
 
  syntax LocalName ::= #varToLocalName(Var)                            [function]
  rule #varToLocalName(V:Var) => #mkLN(#varToString(V))

  syntax LocalNames ::= #varsToLocalNames(VarList)                     [function]
  rule #varsToLocalNames(.VarList) => .LocalNames
  rule #varsToLocalNames(X XS)     => #varToLocalName(X) , #varsToLocalNames(XS)

  syntax String ::= #nameToString(Name)     [function, hook(STRING.token2string)]
                  | #varToString(Var)       [function, hook(STRING.token2string)]

  // Helpers for fresh variables
  // ---------------------------

  syntax GlobalName ::= #intToGlobalName(Int)                          [function]
  syntax LocalName  ::= #intToLocalName(Int)                           [function]
  // ----------------------------------------------------------------------------
  rule #intToLocalName(N)      => #mkLN("fresh" +String Int2String(N))
  rule #intToGlobalName(N:Int) => #mkGN("fresh" +String Int2String(N:Int))

  // Other utilities
  // ---------------

  syntax VarList ::= #reverseVarList(VarList)                          [function]
  syntax VarList ::= #reverseVarListAux(VarList, VarList)              [function]
  // ----------------------------------------------------------------------------
  rule #reverseVarList(VL) => #reverseVarListAux( .VarList , VL)
  rule #reverseVarListAux(OUT, .VarList) => OUT
  rule #reverseVarListAux(OUT, V REM) => #reverseVarListAux(V OUT, REM)

  syntax Int ::= #lengthVarList(VarList)                               [function]
  // ----------------------------------------------------------------------------
  rule #lengthVarList(.VarList) => 0
  rule #lengthVarList(V VL) => 1 +Int #lengthVarList(VL)

  syntax Bool ::= #in(GlobalName, NameList)                            [function]
  // ----------------------------------------------------------------------------
  rule #in(GN, .NameList) => false
  rule #in(GN, N NL) => true
    when GN ==K #nameToGlobalName(N)
  rule #in(GN, N NL) => #in(GN, NL)
    when GN =/=K #nameToGlobalName(N)

endmodule
