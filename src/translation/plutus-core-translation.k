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
      <plutus> #createImplicitCtors
            ~> $PGM:Prg ~> #createDispatch:K
            ~> #translated? ~> #functionsToIele ~> #constructorsToIele
            ~> #moveToContractCell
      </plutus>
      <iele> #staticAlloc
             #staticAppendArg
             .TopLevelDefinitions
      </iele>
      <modName> .K </modName>
      <ieleContract> contract #mkIN("Foo") { .TopLevelDefinitions } </ieleContract>
      <totalFunctions> 0 </totalFunctions>
      <totalReadyForDispatch> 0 </totalReadyForDispatch>
      <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
      <exports> .NameList:NameList </exports>
      <functions>
        <function multiplicity="*" type="Set">
          <fName>   #mkGN("noname")     </fName>
          <params> .VarList:VarList     </params>
          <env>    .Map                 </env>
          <k>      .K:K                 </k>
          <jumpId> -1                   </jumpId>
          <hasDispatchEntry> false:Bool </hasDispatchEntry>
          <readyForDispatch> false:Bool </readyForDispatch>
        </function>
      </functions>
      <constructors>
        <constructor multiplicity="*" type="Set">
          <cName> #mkQualC("Dummy", "NoName") </cName>
          <cId> -1 </cId>
          <arity> -1 </arity>
        </constructor>
      </constructors>
    </T>

  // Create Implicit Constructors
  // ============================

  // Since comparison operators in Plutus use constructors True and False,
  // we need to implicitly define these even if the Prelude is not imported.

  // The constructor ids 1 and 0 correspond to the result of the cmp instruction
  // in IELE so that case matching on booleans work as intended.

  syntax K ::= "#createImplicitCtors"
  rule <plutus> #createImplicitCtors => .K ... </plutus>
       <constructors> ...
         .Bag =>
         <constructor>
           <cName> #mkQualC("Prelude", "True") </cName>
           <cId> 1 </cId>
           <arity> 0 </arity>
         </constructor>
         <constructor>
           <cName> #mkQualC("Prelude", "False") </cName>
           <cId> 0 </cId>
           <arity> 0 </arity>
         </constructor>
       </constructors>

  // Top-Level Functions
  // ===================

  // Unwrap the outer `program`declaration. TODO: Currenty we only support
  // a single module.

  rule <plutus> (program (module MN IS (export (TXS) (NS)) DS) ) => DS ... </plutus>
       <modName> _ => MN </modName>
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
             => #topLevelFunction(GN, JI, X VARS, #replaceFreeVar(T:Tm, X, #length(VARS)))
                ...
       </plutus>
  rule <plutus> #topLevelFunction(GN, JI, VARS:VarList, T:Tm) => .K ... </plutus>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
       <dispatchedFunctionsLeft> DFL => DFL +Int 1 </dispatchedFunctionsLeft>
       <functions> ...
         .Bag =>
         <function>
            <fName> GN </fName>
            <params> #reverse(VARS) </params>
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
  rule <functions>
         ...
          <function>
            <fName> GN </fName>
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

  // For recursion, function name is the same as Name we are loading
  rule <functions>
         ...
          <function>
            <fName> GN </fName>
            <jumpId> JI </jumpId>
            <k> #loadOperand(MN:ModName . N:Name, LN:LocalName)
             => #allocBind(LN, JI, .Map)
             ... </k>
            ...
          </function>
       </functions>
       when #nameToGlobalName(N) ==K GN

  // Data Constructors
  // -----------------

  // For each constructor, we allocate a "constructor ID"
  // We ignore the KSigList since we don't need typing information

  rule <plutus> ( data TYCON ( KSL ) ( CON TL ) ALTS )
             => ( data TYCON ( KSL )            ALTS )
                ...
       </plutus>
       <modName> MN:ModName </modName>
       <constructors> ...
         .Bag =>
         <constructor>
            <cName> MN . CON </cName>
            <cId> !ID:Int </cId>
            <arity> #length(TL) </arity>
         </constructor>
       </constructors>

  rule <plutus> ( data TYCON ( KSL ) .AltList ) => .K ... </plutus>

  // TODO: Since `#useOperand` is a function, we cannot just use the
  // construct id from `<constructors>`

  // Unfortunately, ValList are not subsorts of TmLists so we need two separate rules
  rule #useOperand((con MN:ModName . TYCON TL:TmList),  LN) => LN
  rule #useOperand((con MN:ModName . TYCON VL:ValList), LN) => LN

  rule #loadOperand((con QC .ValList), LN) => LN = call #qualCToGlobalName(QC)(.Operands) .Instructions
  rule #loadOperand((con QC V:Val), LN)
    => #concatInsList( #loadOperand(V, #intToLocalName(!N))
                     , LN = call #qualCToGlobalName(QC)(#useOperand(V, #intToLocalName(!N)))
                       .Instructions
                     )

  // Case Statements
  // ---------------

  rule #useOperand((case T CLS), LN) => LN
  rule #loadOperand((case T CLS), LN)
    => #concatInsList( #loadOperand(T, #intToLocalName(!N))
                     , #translateCases(CLS, #useOperand(T, #intToLocalName(!N)), LN, #mkIN("end", !M))
                     , #label(#mkIN("end", !M)) .Instructions)

  syntax PreInstructions ::= #translateCases(ClList, Operand, LocalName, IeleName)
                           | #compareAndJump(Cl, Operand, IeleName)
                           | #labelAndLoad(Cl, Operand, LocalName, IeleName, IeleName)

  rule #translateCases(.ClList, OP, RET, END) => call #mkGN("iele.invalid")(.Operands) .Instructions
  rule #translateCases(CL CLS, OP, RET, END)
    => #concatInsList( #compareAndJump(CL, OP, #mkIN("match", !N))
                     , #translateCases(CLS, OP, RET, END)
                     , #labelAndLoad(CL, OP, RET, #mkIN("match", !N), END))

  // TODO: correctly handle VarList VS
  rule <k> #compareAndJump((QC (VS) RHS), OP, MATCH)
        => #intToLocalName(!COND) = cmp eq I, OP
           br #intToLocalName(!COND), MATCH
          .Instructions ...
       </k>
       <constructor>
         <cName> QC </cName>
         <cId> I </cId>
         ...
       </constructor>

  rule #labelAndLoad((QC (VS) RHS), OP, RET, MATCH, END)
    => #concatInsList( #label(MATCH) .Instructions
                     , #loadOperand(RHS, RET)
                     , RET = #useOperand(RHS, RET)
                       br END)

  // Builtins
  // --------

  // Binary builtins share code for `#loadOperand` and `#useOperand`.

  rule #useOperand((builtin B:BinaryBuiltin  T1:Tm T2:Tm), LN) => LN
  rule #loadOperand((builtin B:BinaryBuiltin T1:Tm T2:Tm), LN)
    => #concatInsList(#loadOperand(T1, #intToLocalName(!N1)),
                      #loadOperand(T2, #intToLocalName(!N2)),
                      #translateBinBuiltin( B:BinaryBuiltin, LN
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
  
  syntax Tm     ::= #freeVar(Var, Int)
  syntax Tm     ::= #replaceFreeVar(Tm, Var, Int)                      [function]
  syntax ClList ::= #replaceFreeVarClL(ClList, Var, Int)               [function]
  syntax TmList ::= #replaceFreeVarTL(TmList, Var, Int)                [function]
  
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

  rule #replaceFreeVar((case T CLS), Y:Var, INDEX)
    => (case #replaceFreeVar(T, Y, INDEX) #replaceFreeVarClL(CLS, Y, INDEX))
  rule #replaceFreeVarClL(.ClList, X, INDEX) => .ClList
  rule #replaceFreeVarClL((QC (VS) T) CLS, X, INDEX)
    => (QC (VS) #replaceFreeVar(T, X, INDEX)) #replaceFreeVarClL(CLS, X, INDEX)

  // When replacing free vars in a TermList, we replace the free var for all terms in that list.
  // If the list contains only Vals, then there will be no free vars, so we just return the ValList

  rule #replaceFreeVar((con MN:ModName . TYCON TL:TmList), Y, INDEX)
    => (con MN . TYCON #replaceFreeVarTL(TL, Y, INDEX))
  rule #replaceFreeVar((con MN:ModName . TYCON VL:ValList), Y, INDEX)
    => (con MN . TYCON VL)
  rule #replaceFreeVarTL(.TmList, X, INDEX) => .TmList
  rule #replaceFreeVarTL(T TL, X, INDEX)
    => #replaceFreeVar(T, X, INDEX) #replaceFreeVarTL(TL, X, INDEX)
  
  // For `#freeVar`s `#loadOperand` directly uses the function's parameter.
  // TODO: Make use of INDEX to prevent collisions in case of non-unique argument names.

  rule #loadOperand(#freeVar(V, INDEX), LN) => .Instructions
  rule #useOperand(#freeVar(V, INDEX), LN) => #varToLocalName(V) 
  
  // Lambda Applications
  // -------------------
  
  // Lambda applications simply pass the closure generated by `#allocBind` to
  // `#mkGN("dispatch")`.
  
   rule #loadOperand([T1:Tm T2:Tm], LN)
     => #concatInsList( #loadOperand(T1, #intToLocalName(!N))
                      , #loadOperand(T2, #intToLocalName(!M))
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
                           <fName>   (#mkGN("dispatch")):GlobalName </fName>
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
          <fName>  FN </fName>
          <jumpId> JI </jumpId>
          <hasDispatchEntry> false:Bool => true:Bool </hasDispatchEntry>
          <readyForDispatch> true:Bool               </readyForDispatch>
          <params> PARAMS                            </params>
          ...
       </function>
       <function>
         <k> #dispatchJumpTable:PreInstructions
                => #concatInsList( #dispatchCheckCond(JI, FN, #length(PARAMS))
                                 , #dispatchJumpTable
                                 , #dispatchCall(JI, FN, #length(PARAMS)))
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

  // Checks that all functions have been converted

  syntax K ::= "#translated?"
  rule <plutus> #translated? => .K ... </plutus>
       <dispatchedFunctionsLeft> 0 </dispatchedFunctionsLeft>
       <totalReadyForDispatch> TF </totalReadyForDispatch>
       <totalFunctions> TF </totalFunctions>

  // Convert functions to IELE

  syntax K ::= "#functionsToIele"

  // TODO: streamline converting public and private functions to IELE
  // Convert public `<function>`s to IELE syntax
  rule <plutus> #functionsToIele ... </plutus>
       <functions>
         <function>
           <fName>  GN:GlobalName   </fName>
           <params> VARS:VarList    </params>
           <env>    ENV             </env>
           <k>      IS:Instructions </k>
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

  // Convert private `<function>`s to IELE syntax
  rule <plutus> #functionsToIele ... </plutus>
       <functions>
         <function>
           <fName>  GN:GlobalName   </fName>
           <params> VARS:VarList    </params>
           <env>    ENV             </env>
           <k>      IS:Instructions </k>
           ...
         </function> => .Bag
         ...
       </functions>
       <iele> TLDS:TopLevelDefinitions
           => (define GN (#varsToLocalNames(VARS)) { IS .LabeledBlocks }):FunctionDefinition TLDS
       </iele>
       <exports> NL </exports>
       when notBool #in(GN, NL)

  rule <plutus> #functionsToIele => .K ... </plutus>
       <functions> .Bag </functions>

  // When there are no functions left, add IELE functions for each constructor

  syntax K ::= "#constructorsToIele"
  rule <plutus> #constructorsToIele ... </plutus>
       <constructors>
         <constructor>
           <cName> QC </cName>
           <cId> CID </cId>
           ...
         </constructor> => .Bag
         ...
       </constructors>
       <iele> TLDS:TopLevelDefinitions
           => define #qualCToGlobalName(QC) ( .LocalNames ) {
                ret CID
                .Instructions .LabeledBlocks
              }
              TLDS
       </iele>

  rule <plutus> #constructorsToIele => .K ... </plutus>
       <constructors> .Bag </constructors>

  // When there are no functions or constructors left, return a top-level contract

  syntax K ::= "#moveToContractCell"
  rule <functions> .Bag </functions>
       <constructors> .Bag </constructors>
       <plutus> #moveToContractCell => .K ... </plutus>
       <iele> TLDS:TopLevelDefinitions => .TopLevelDefinitions </iele>
       <ieleContract> _ => contract #mkIN("Foo") { TLDS } </ieleContract>

  // Helpers for ...
  // ===============
  
  // ... generating `Instruction`s
  // ------------------------------
  
   syntax PreInstructions ::= Instructions
   syntax KResult         ::= Instructions
  
  syntax PreInstructions
      ::= #concatInsList(PreInstructions, PreInstructions)               [strict]
        | #concatInsList(PreInstructions, PreInstructions, PreInstructions) [klabel(concatInsList3)]

  rule #concatInsList(IS:Instructions, .Instructions) => IS:Instructions
  rule #concatInsList(IS:Instructions, I IS')
    => #concatInsList(#addInsToBack(IS:Instructions, I:Instruction .Instructions), IS')
  rule #concatInsList(IS1, IS2, IS3)
    => #concatInsList(IS1, #concatInsList(IS2, IS3))

  syntax Instructions ::= #addInsToBack(Instructions, Instructions)    [function]
  rule #addInsToBack(.Instructions, IS) => IS:Instructions
  rule #addInsToBack(I IS, IS') => I #addInsToBack(IS, IS')
  
  // TODO: We should be using `Block`s instead of `Instructions` in some places
  // TODO: Rename to `#label`

  // "Cheats" for making generating some tokens easier
  // -------------------------------------------------
  syntax Instruction ::= #label(IeleName)                         [format(%n%3:)]
  syntax Instruction ::= #comment(String)                       [format(%n// %3)]

  // Converting strings to {Iele,Local,Global}Name, QualCs, and Vars
  // ---------------------------------------------------------------

  syntax GlobalName ::= #mkGN(String)                                  [function]
  syntax LocalName  ::= #mkLN(String)                                  [function]
  syntax IeleName   ::= #mkIN(String)                                  [function]
                      | #mkIN(String, Int)            [function, klabel(mkIN_SI)]
  syntax Var        ::= #mkVar(String)                                 [function]
  syntax QualC      ::= #mkQualC(String, String)                       [function]
  // ----------------------------------------------------------------------------
  rule #mkGN(S)    => #parseToken("GlobalName", "@" +String S)
  rule #mkLN(S)    => #parseToken("LocalName",  "%" +String S)
  rule #mkIN(S)    => #parseToken("IeleName",               S)
  rule #mkIN(S, I) => #parseToken("IeleName", S +String Int2String(I))
  rule #mkVar(S)   => #parseToken("Var",                    S)

  // K does not let us make a qualC in one fail swoop using #parseToken(..) . #parseToken(..).
  // Instead, we need to use #parseToken and hold the information needed to make a QualC
  // and pass it to an external #qualCHolder function which puts it together.
  syntax QualC ::= #qualCHolder(K, K)                                  [function]
  rule #mkQualC(MN, C) => #qualCHolder(#parseToken("UpperName", MN), #parseToken("UpperName", C))
  rule #qualCHolder(MN, C) => (MN . C):QualC

  // Converting Names, Vars to GlobalNames, LocalNames, Strings
  // ----------------------------------------------------------

  syntax GlobalName ::= #nameToGlobalName(Name)                        [function]
                      | #qualCToGlobalName(QualC)                      [function]
  rule #nameToGlobalName(N:Name) => #mkGN(#nameToString(N))
  rule #qualCToGlobalName(MN . C)
    => #mkGN(#modNameToString(MN) +String "." +String #conToString(C))
 
  syntax LocalName ::= #varToLocalName(Var)                            [function]
  rule #varToLocalName(V:Var) => #mkLN(#varToString(V))

  syntax LocalNames ::= #varsToLocalNames(VarList)                     [function]
  rule #varsToLocalNames(.VarList) => .LocalNames
  rule #varsToLocalNames(X XS)     => #varToLocalName(X) , #varsToLocalNames(XS)

  syntax String ::= #nameToString(Name)     [function, hook(STRING.token2string)]
                  | #varToString(Var)       [function, hook(STRING.token2string)]
                  | #conToString(Con)       [function, hook(STRING.token2string)]
                  | #modNameToString(ModName)
                                            [function, hook(STRING.token2string)]

  // Helpers for fresh variables
  // ---------------------------

  syntax GlobalName ::= #intToGlobalName(Int)                          [function]
  syntax LocalName  ::= #intToLocalName(Int)                           [function]
  // ----------------------------------------------------------------------------
  rule #intToLocalName(N)      => #mkLN("fresh" +String Int2String(N))
  rule #intToGlobalName(N:Int) => #mkGN("fresh" +String Int2String(N:Int))

  // Other utilities
  // ---------------

  syntax VarList ::= #reverse(VarList)         [function, klabel(reverseVarList)]
  syntax VarList ::= #reverseAux(VarList, VarList)
                                            [function, klabel(reverseVarListAux)]
  // ----------------------------------------------------------------------------
  rule #reverse(VL) => #reverseAux(.VarList, VL)
  rule #reverseAux(OUT, .VarList) => OUT
  rule #reverseAux(OUT, V REM) => #reverseAux(V OUT, REM)

  syntax Int ::= #length(VarList)               [function, klabel(lengthVarList)]
               | #length(TyList)                 [function, klabel(lengthTyList)]
  // ----------------------------------------------------------------------------
  rule #length(.VarList) => 0
  rule #length(V VL:VarList) => 1 +Int #length(VL)

  rule #length(.TyList) => 0
  rule #length(T TL:TyList) => 1 +Int #length(TL)

  syntax Bool ::= #in(GlobalName, NameList)                            [function]
  // ----------------------------------------------------------------------------
  rule #in(GN, .NameList) => false
  rule #in(GN, N NL) => true
    when GN ==K #nameToGlobalName(N)
  rule #in(GN, N NL) => #in(GN, NL)
    when GN =/=K #nameToGlobalName(N)

endmodule
