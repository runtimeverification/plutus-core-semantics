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
          <cK> .K:K </cK>
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
           <cK> #translateIeleCtor </cK>
         </constructor>
         <constructor>
           <cName> #mkQualC("Prelude", "False") </cName>
           <cId> 0 </cId>
           <arity> 0 </arity>
           <cK> #translateIeleCtor </cK>
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
  // TODO: Avoid name collisions when `#bind`inv variables
  rule <plutus> #topLevelFunction(GN, JI, VARS:VarList, (lam X:Var T:Tm))
             => #topLevelFunction(GN, JI, X VARS, #bindVar(T:Tm, X, #varToLocalName(X)))
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
  // This is done via `#loadOperand` and `#useOperand`. The process must also be able
  // to recurse into each term to bind free variables.

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

  // Integers are not affected by substituion.

  rule #bindVar(I:Int, X    , BLN) => I:Int

  // A variable is bound if it matches the variable we are interested in.

  rule #bindVar(X:Var, X    , BLN) => #boundVar(BLN)
  rule #bindVar(X:Var, Y:Var, BLN) => X:Var          when (X:Var =/=K Y:Var)

  // Qualified Names
  // ---------------

  // Since using a qualified name involves binding a closure, we assign it
  // to the `LocalName` provided.

  rule #useOperand(QN:QualN, LN) => LN

  // To generate the closure to call the function, we grab its `jumpId`
  // and create the closure.

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

  // Unfortunately, K's matching syntax means that we need to duplicate the
  // rule for the recursive case.

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

  // We do not need bind free variables.

  rule #bindVar(QN:QualN, X:Var, BLN) => QN

  // Data Declarations
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
           <cK> #translateIeleCtor </cK>
           <cId> !ID:Int </cId>
           <arity> #length(TL) </arity>
         </constructor>
       </constructors>

  syntax K ::= "#translateIeleCtor"
  rule <constructor>
         <cK> #translateIeleCtor
           => #mkLN("ret") = call #mkGN("alloc")(ARITY)
              #concatInsListFct( #bindToTerm(ARITY)
                               , #returnTerm(ARITY, CID)
                               )
         </cK>
         <cId> CID </cId>
         <arity> ARITY </arity>
         ...
       </constructor>

  syntax Instructions ::= #bindToTerm(Int)                             [function]
                        | #returnTerm(Int, Int)                        [function]

  rule #bindToTerm(0) => .Instructions
  rule #bindToTerm(ARITY)
    => #concatInsListFct( #bindToTerm(ARITY -Int 1)
                        , #mkLN("subtermPtr") = add #mkLN("ret"), ARITY -Int 1
                          store #mkLN("subtermPtr"), #mkLN("param" +String Int2String(ARITY))
                         .Instructions
                        )
    when ARITY >Int 0

  rule #returnTerm(ARITY, CID)
    => #mkLN("ret") = shift #mkLN("ret"), 8
       #mkLN("ret") = or #mkLN("ret"), CID
       ret #mkLN("ret")
    when CID <Int 2 ^Int 8

  rule <plutus> ( data TYCON ( KSL ) .AltList ) => .K ... </plutus>

  // Data Constructors
  // -----------------

  // Unfortunately, ValList are not subsorts of TmLists so we need two separate rules
  // Since `#useOperand` is a function, we cannot just use the
  // constructor id as on `Operand`.

  rule #useOperand((con QC TL:TmList),  LN) => LN
  rule #useOperand((con QC VL:ValList), LN) => LN

  // Since each Data declaration generated a function generated from its
  // qualified name, we simply call that function.

  rule #loadOperand((con QC .ValList), LN) => LN = call #qualCToGlobalName(QC)(.Operands) .Instructions
  rule #loadOperand((con QC V:Val), LN)
    => #concatInsList( #loadOperand(V, #mkLN("op", !N))
                     , LN = call #qualCToGlobalName(QC)(#useOperand(V, #mkLN("op", !N)))
                       .Instructions
                     )

  // K's parsing means that ValLists and TermLists are distinct unrelated sorts.
  // If the list contains only Vals, then there will be no free vars, so we
  // just return the ValList

  rule #bindVar((con QC VL:ValList), Y, BLN)
    => (con QC VL)

  // Otherwise, we recurse into the list.

  rule #bindVar((con QC TL:TmList), Y, BLN)
    => (con QC #bindVarTL(TL, Y, BLN))

  // Case Statements
  // ---------------

  // Case statements generate a switch statement like structure.
  // We check if the operand we are matching on matches each case.

  rule #loadOperand((case T CLS), LN)
    => #concatInsList( #loadOperand(T, #mkLN("case", !N))
                     , #translateCases(CLS, #useOperand(T, #mkLN("case", !N)), LN, #mkIN("end", !M))
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
  // TODO: Calculating the constuctor id of the operand needs to be done just
  // once.
  // If the operands ctor matches the constructor's id, we jump to that case...

  rule <k> #compareAndJump((QC (VS) RHS), OP, MATCH)
        => #mkLN("ctor", !COND) = and OP, (2 ^Int 8 -Int 1)
           #mkLN("cond", !COND) = cmp eq CID, #mkLN("ctor", !COND)
           br #mkLN("cond", !COND), MATCH
          .Instructions ...
       </k>
       <constructor>
         <cName> QC </cName>
         <cId> CID </cId>
         ...
       </constructor>

  // ... where we translate the body of the case statement

  rule #labelAndLoad((QC (VS) RHS), OP, RET, MATCH, END)
    => #concatInsList( #label(MATCH) .Instructions
                     , #loadOperand(RHS, RET)
                     , RET = #useOperand(RHS, RET)
                       br END)

  rule #useOperand((case T CLS), LN) => LN

  rule #bindVar((case T CLS), Y:Var, BLN)
    => (case #bindVar(T, Y, BLN) #bindVarClL(CLS, Y, BLN))
  rule #bindVarClL(.ClList, X, BLN) => .ClList

  // TODO: we should not recurse if `X` is in `VS`

  rule #bindVarClL((QC (VS) T) CLS, X, BLN)
    => (QC (VS) #bindVar(T, X, BLN)) #bindVarClL(CLS, X, BLN)

  // Builtins
  // --------

  // Binary builtins share code for `#loadOperand` and `#useOperand`.

  rule #loadOperand((builtin B:BinaryBuiltin T1:Tm T2:Tm), LN)
    => #concatInsList(#loadOperand(T1, #mkLN("bltinN1", !N1)),
                      #loadOperand(T2, #mkLN("bltinN1", !N2)),
                      #translateBinBuiltin( B:BinaryBuiltin, LN
                                          , #useOperand(T1, #mkLN("bltinN1", !N1))
                                          , #useOperand(T2, #mkLN("bltinN2", !N2))))

  rule #useOperand((builtin B:BinaryBuiltin  T1:Tm T2:Tm), LN) => LN

  rule #bindVar((builtin N T1 T2), Y:Var, BLN)
    => (builtin N #bindVar(T1, Y, BLN) #bindVar(T2, Y, BLN))

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
  //    the parameters as `#boundVar`s, so they can correctly access their value
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
       <plutus> (.K => #topLevelFunction(#mkGN("innerLambda", !N), !N, X, T:Tm)) ... </plutus>

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
                                          #mkLN("bindPtr", !N) = add LN, (OFF +Int 1)
                                          store #varToLocalName(V), #mkLN("bindPtr", !N)
                                          #bindEnv(M, LN)
                                         ):Instructions
  
  // `#addParamsToEnv` is a helper for building the environment map:
  // TODO: Should this really be called an environment?
  
  syntax Map ::= #addParamsToEnv(VarList, Map)                         [function]
  rule #addParamsToEnv(X:Var XS, MAP:Map) => #addParamsToEnv(XS, (size(MAP) |-> X:Var):Map MAP:Map)
  rule #addParamsToEnv(.VarList, MAP:Map) => MAP:Map

  rule #useOperand((lam X:Var T:Tm), LN:LocalName) => LN:LocalName

  // If an inner lambda wants to use the same variable name, we should not
  // recurse into that lambda.

  rule #bindVar((lam X:Var T), X:Var, BLN) => ((lam X:Var T)):Tm
  rule #bindVar((lam X:Var T), Y:Var, BLN) => ((lam X:Var #bindVar(T, Y, BLN))):Tm
    when (X:Var =/=K Y:Var)

  // Binding free variables
  // ----------------------

  syntax Tm     ::= #boundVar(LocalName)
  syntax Tm     ::= #bindVar(Tm, Var, LocalName)                       [function]
  syntax ClList ::= #bindVarClL(ClList, Var, LocalName)                [function]
  syntax TmList ::= #bindVarTL(TmList, Var, LocalName)                 [function]
  
  rule #bindVar(#boundVar(J), X, BLN) => #boundVar(J)

  // When replacing free vars in a TermList, we replace the free var for all terms in that list.

  rule #bindVarTL(.TmList, X, BLN) => .TmList
  rule #bindVarTL(T TL, X, BLN)
    => #bindVar(T, X, BLN) #bindVarTL(TL, X, BLN)
  
  // For `#boundVar`s `#loadOperand` directly uses the function's parameter.

  rule #loadOperand(#boundVar(BLN), LN) => .Instructions
  rule #useOperand(#boundVar(BLN), LN) => BLN 
  
  // Lambda Applications
  // -------------------
  
  // Lambda applications simply pass the closure generated by `#allocBind` to
  // `#mkGN("dispatch")`.
  
   rule #loadOperand([T1:Tm T2:Tm], LN)
     => #concatInsList( #loadOperand(T1, #mkLN("appT1", !N))
                      , #loadOperand(T2, #mkLN("appT2", !M))
                      , LN = call #mkGN("dispatch")( #useOperand(T1, #mkLN("appT1", !N))
                                                   , #useOperand(T2, #mkLN("appT2", !M)))
                        .Instructions
                      )
  rule #useOperand([T1 T2], LN:LocalName) => LN:LocalName

  rule #bindVar([T1 T2], X:Var, BLN)
    => [#bindVar(T1, X, BLN) #bindVar(T2, X, BLN)]
  
  // Generate static functions
  // =========================

  syntax FunctionDefinition ::= "#staticAlloc"                         [function]
  rule #staticAlloc => define #mkGN("alloc") (#mkLN("size") , .LocalNames) {
                         #mkLN("isZero") = cmp eq #mkLN("size"), 0
                         br #mkLN("isZero"), #mkIN("allocZero")
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
                      #label(#mkIN("allocZero"))
                         ret 0
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
           <cK> IS:Instructions </cK>
           <cId> CID </cId>
           <arity> ARITY </arity>
         </constructor> => .Bag
         ...
       </constructors>
       <iele> TLDS:TopLevelDefinitions
           => define #qualCToGlobalName(QC) ( #createArgs(ARITY) ) {
                IS
                .LabeledBlocks
              }
              TLDS
       </iele>

  syntax LocalNames ::= #createArgs(Int)                              [function]
  rule #createArgs(0) => .LocalNames
  rule #createArgs(I) => #mkLN("param" +String Int2String(I)), #createArgs(I -Int 1)
    when I >Int 0

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

  syntax Instructions ::= #concatInsListFct(Instructions, Instructions)  [function]

  rule #concatInsList(IS:Instructions, .Instructions) => IS:Instructions
  rule #concatInsList(IS:Instructions, I IS')
    => #concatInsList(#addInsToBack(IS:Instructions, I:Instruction .Instructions), IS')
  rule #concatInsList(IS1, IS2, IS3)
    => #concatInsList(IS1, #concatInsList(IS2, IS3))

  rule #concatInsListFct(.Instructions, IS) => IS
  rule #concatInsListFct(I IS, IS') => I #concatInsListFct(IS, IS')

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
                      | #mkGN(String, Int)            [function, klabel(mkGN_SI)]
  syntax LocalName  ::= #mkLN(String)                                  [function]
                      | #mkLN(String, Int)            [function, klabel(mkLN_SI)]
  syntax IeleName   ::= #mkIN(String)                                  [function]
                      | #mkIN(String, Int)            [function, klabel(mkIN_SI)]
  syntax Var        ::= #mkVar(String)                                 [function]
  syntax QualC      ::= #mkQualC(String, String)                       [function]
  // ----------------------------------------------------------------------------
  rule #mkGN(S)    => #parseToken("GlobalName", "@" +String S)
  rule #mkLN(S)    => #parseToken("LocalName",  "%" +String S)
  rule #mkIN(S)    => #parseToken("IeleName",               S)
  rule #mkVar(S)   => #parseToken("Var",                    S)

  rule #mkGN(S, I) => #mkGN(S +String Int2String(I))
  rule #mkLN(S, I) => #mkLN(S +String Int2String(I))
  rule #mkIN(S, I) => #mkIN(S +String Int2String(I))

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
