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
      <plutus> $PGM:Prg
            ~> #translated?
            ~> #functionsToIele ~> #constructorsToIele
            ~> #moveToContractCell
      </plutus>
      <iele> #staticAlloc
             #staticAppendArg
             #staticDispatch
             #staticUnfold(2)
             #staticUnfold(3)
             #staticUnfold(4)
             .TopLevelDefinitions
      </iele>
      <currMod> .K </currMod>
      <ieleContract> contract #mkIN("Foo") { .TopLevelDefinitions } </ieleContract>
      <totalFunctions> 0 </totalFunctions>
      <totalTranslated> 0 </totalTranslated>
      <exports> .NameList:NameList </exports>
      <functions>
        <function multiplicity="*" type="Set">
          <modName> .K                  </modName>
          <fName>   .K                  </fName>
          <params> .VarList:VarList     </params>
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

  // Tuneable knobs
  // ==============

  // Number of bytes to reserve for constructor IDs and jump IDs in their representations
  syntax Int ::= "#CidOffset"                                          [function]
               | "#CidOffsetBits"                                      [function]
               | "#CidWidth"                                           [function]
               | "#CidWidthBits"                                       [function]
               | "#CidBitMask"                                         [function]
               | "#CidMax"                                             [function]
               | "#numArgsOffset"                                      [function]
               | "#numArgsWidth"                                       [function]
               | "#JIWidth"                                            [function]
               | "#JIWidthBits"                                        [function]
  
  rule #CidOffset     => 2
  rule #CidOffsetBits => 8 *Int #CidOffset
  rule #CidWidth      => 2
  rule #CidWidthBits  => 8 *Int #CidWidth
  rule #CidMax        => 2^Int #CidWidthBits
  rule #CidBitMask    => #CidMax -Int 1
  rule #numArgsOffset => 2
  rule #numArgsWidth  => 2
  rule #JIWidth       => 2
  rule #JIWidthBits   => 8 *Int #JIWidth

  // Typing Related Constructs
  // =========================

  // Ignore `Dec`s related to typing
  rule <plutus> _:TDec => .K ... </plutus>
  rule <plutus> _:MDec => .K ... </plutus>

  // Ignore `abs`
  rule #useOperand(  (abs _ T), LN ) => #useOperand(T, LN)
  rule #loadOperand( (abs _ T), LN ) => #loadOperand(T, LN)
  rule #bindVar( (abs _ T), V, LN ) => #bindVar(T, V, LN)

  // Ignore `inst`
  rule #useOperand(  (inst T _), LN ) => #useOperand(T, LN)
  rule #loadOperand( (inst T _), LN ) => #loadOperand(T, LN)
  rule #bindVar( (inst T _), V, LN ) => #bindVar(T, V, LN)

  // Top-Level Functions
  // ===================

  // Unwrap the outer `program`declaration.

  rule <plutus> (program MOD MODS) => MOD ~> (program MODS) ... </plutus>
  rule <plutus> (program .ModList) => .K                    ... </plutus>

  rule <plutus> (module MN IS (export (TXS) (NS)) DS) => DS ... </plutus>
       <currMod> _ => MN </currMod>
       <exports> _ => NS </exports>

  // We iterate over `Dec`s, creating a named top level function for each.

  rule <plutus> D:Dec DS:DecList => D ~> DS ... </plutus>

  rule <plutus> (define NAME:Name V:Val)
             => #topLevelFunction(NAME, !JI, .VarList, V)
                ...
       </plutus>
  rule <plutus> .DecList => .K ... </plutus>

  syntax K ::= #topLevelFunction(Name, Int, VarList, Tm)
  // TODO: Avoid name collisions when `#bind`ing
  rule <plutus> #topLevelFunction(NAME, JI, VARS:VarList, (lam X:Var T:Tm))
             => #topLevelFunction(NAME, JI, X VARS, #bindVar(T:Tm, X, #varToLocalName(X)))
                ...
       </plutus>
  rule <plutus> #topLevelFunction(NAME, JI, VARS:VarList, T:Tm) => .K ... </plutus>
       <totalFunctions> TF:Int => TF:Int +Int 1 </totalFunctions>
       <currMod> MN </currMod>
       <functions> ...
         .Bag =>
         <function>
            <modName> MN </modName>
            <fName> NAME </fName>
            <params> #reverse(VARS) </params>
            <jumpId> JI </jumpId>
            <k>      #concatInsList(#loadOperand(T:Tm, #mkLN("ret")),
                                    ret #useOperand(T:Tm, #mkLN("ret")) .Instructions)
            </k>
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

  // Some constructs need to load / use `TmList`s

  syntax PreInstructions ::= #loadOperands(TmList, LocalNames)         [function]
  // ----------------------------------------------------------------------------
  rule #loadOperands(T TL, (LN, LNS))
    => #concatInsList(#loadOperand(T, LN), #loadOperands(TL, LNS))
  rule #loadOperands(.TmList, .LocalNames) => .Instructions

  syntax Operands ::= #useOperands(TmList, LocalNames)                 [function]
  // ----------------------------------------------------------------------------
  rule #useOperands(T TL, (LN, LNS))
    => #useOperand(T, LN), #useOperands(TL, LNS)
  rule #useOperands(.TmList, .LocalNames) => .Operands


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
            <modName> MN </modName>
            <fName> NAME </fName>
            ...
          </function>
          <function>
            <k> #loadOperand(MN:ModName . NAME:Name, LN:LocalName)
             => #allocBind(LN, MN, NAME, .VarList)
             ... </k>
            ...
          </function>
       </functions>

  // Unfortunately, K's matching syntax means that we need to duplicate the
  // rule for the recursive case.

  rule <functions>
         ...
          <function>
            <modName> MN </modName>
            <fName> NAME </fName>
            <params> VARS </params>
            <k> #loadOperand(MN:ModName . NAME:Name, LN:LocalName)
             => #allocBind(LN, MN, NAME, .VarList)
             ... </k>
            ...
          </function>
       </functions>

  // We do not need bind free variables.

  rule #bindVar(QN:QualN, X:Var, BLN) => QN

  // Data Declarations
  // -----------------

  // For each constructor, we allocate a "constructor ID"
  // We ignore the KSigList since we don't need typing information

  rule <plutus> ( data TYCON ( KSL ) .AltList ) => .K ... </plutus>
  rule <plutus> ( data TYCON ( KSL ) ( CON TL ) ALTS )
             => ( data TYCON ( KSL )            ALTS )
                ...
       </plutus>
       <currMod> MN:ModName </currMod>
       <constructors> ...
         .Bag =>
         <constructor>
           <cName> MN . CON </cName>
           <cK> #translateIeleCtor </cK>
           <cId> #mkConId(MN . CON) </cId>
           <arity> #length(TL) </arity>
         </constructor>
       </constructors>

  syntax Int ::= #mkConId(QualC)                                       [function]
  rule #mkConId(QC) => 0
    when #mkQualC("Prelude", "False") ==K QC
  rule #mkConId(QC) => 1
    when #mkQualC("Prelude", "True") ==K QC
  rule #mkConId(QC) => !ID:Int                                            [owise]

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
                          store #mkLN("param" +String Int2String(ARITY)), #mkLN("subtermPtr")
                         .Instructions
                        )
    when ARITY >Int 0

  rule #returnTerm(ARITY, CID)
    => #mkLN("ret") = shift #mkLN("ret"), #CidOffsetBits
       #mkLN("ret") = or #mkLN("ret"), CID
       ret #mkLN("ret")
    when CID <Int #CidMax

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

  // To generate multiple fresh `LocalName`s we need an auxilary function.

  rule #loadOperand((con QC TL:TmList), LN) => #loadOperandDCAux(QC, TL, #mkLNs(#length(TL), "ops"), LN) 

  // K's parsing means that ValLists and TermLists are distinct unrelated sorts.
  // If the list contains only Vals, then there will be no free vars, so we
  // just return the ValList

  rule #loadOperand((con QC VL:ValList          ), LN)
    => #loadOperand((con QC #valListToTmList(VL)), LN)

  syntax PreInstructions ::= #loadOperandDCAux(QualC, TmList, LocalNames, LocalName)
  rule #loadOperandDCAux(QC, TL, LNS, LN)
    => #concatInsList( #loadOperands(TL, LNS)
                     , LN = call #qualCToGlobalName(QC)(#useOperands(TL, LNS))
                       .Instructions
                     )

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
        => #mkLN("ctor", !COND) = and OP, #CidBitMask
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
    => #concatInsList( #label(MATCH)
                       OP = shift OP, (0 -Int #CidWidthBits)
                       .Instructions
                     , #labelAndLoadDeconstruct(VS, RHS, OP, 0, RET)
                     , br END
                       .Instructions)

  syntax PreInstructions ::= #labelAndLoadDeconstruct(VarList, Tm, Operand, Int, LocalName)
  rule #labelAndLoadDeconstruct(V VL, RHS, OP, OFFSET, RET)
    => #concatInsList( #mkLN("subtermAddr", !N) = add OP, OFFSET
                       #mkLN("matchOp", !N) = load #mkLN("subtermAddr", !N)
                       .Instructions
                     , #labelAndLoadDeconstruct(VL, #bindVar(RHS, V, #mkLN("matchOp", !N)), OP, OFFSET +Int 1, RET)
                     )
  rule #labelAndLoadDeconstruct(.VarList, RHS, OP, OFFSET, RET)
    => #concatInsList( #loadOperand(RHS, #mkLN("match", !N))
                     , RET = #useOperand(RHS, #mkLN("match", !N))
                       .Instructions)

  rule #useOperand((case T CLS), LN) => LN

  rule #bindVar((case T CLS), Y:Var, BLN)
    => (case #bindVar(T, Y, BLN) #bindVarClL(CLS, Y, BLN))
  rule #bindVarClL(.ClList, X, BLN) => .ClList

  // TODO: we should not recurse if `X` is in `VS`

  rule #bindVarClL((QC (VS) T) CLS, X, BLN)
    => (QC (VS) #bindVar(T, X, BLN)) #bindVarClL(CLS, X, BLN)

  // Builtins
  // --------

  // Unary builtins

  rule #loadOperand((builtin B:UnaryBuiltin T1:Tm), LN)
    => #concatInsList(#loadOperand(T1, #mkLN("bltinN1", !N1)),
                      #translateUnaryBuiltin( B:UnaryBuiltin, LN
                                            , #useOperand(T1, #mkLN("bltinN1", !N1))))
  rule #useOperand((builtin B:UnaryBuiltin  T1:Tm), LN) => LN
  rule #bindVar((builtin N T1), Y:Var, BLN)
    => (builtin N #bindVar(T1, Y, BLN))

  syntax Instructions ::= #translateUnaryBuiltin(UnaryBuiltin, LocalName, Operand) [function]
  // -------------------------------------------------------------------------------------------------

  // Binary builtins share code for `#loadOperand` and `#useOperand`.

  rule #loadOperand((builtin B:BinaryBuiltin T1:Tm T2:Tm), LN)
    => #concatInsList(#loadOperand(T1, #mkLN("bltinN1", !N1)),
                      #loadOperand(T2, #mkLN("bltinN2", !N2)),
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
             => #allocBind(LN, MN, #mkPN("innerLambda", !N), VS)
             ... </k>
            <params> VS </params>
            ...
          </function>
       </functions>
       <currMod> MN </currMod>
       <plutus> (.K => #topLevelFunction(#mkPN("innerLambda", !N), !N, X, T:Tm)) ... </plutus>

  // `#allocBind` generates instructions to allocate a closure and bind its
  // environment.

  syntax PreInstructions ::= #allocBind(LocalName, ModName, Name,      VarList)
  syntax PreInstructions ::= #allocBind(LocalName, ModName, Name, Int, VarList)
                                                     [klabel(allocBindWithArity)]
  syntax Instructions ::= #bindEnv(LocalName, VarList)                 [function]

  rule <k> #allocBind(N, MN:ModName, FN:Name,                  FVS)
        => #allocBind(N, MN:ModName, FN:Name, #length(PARAMS), FVS)
           ...
       </k>
       <function>
         <modName> MN </modName>
         <fName>   FN </fName>
         <params>  PARAMS </params>
         ...
       </function>

  rule  <function>
         <k> #allocBind(N, MN:ModName, FN:Name,                  FVS)
          => #allocBind(N, MN:ModName, FN:Name, #length(PARAMS), FVS)
             ...
         </k>
         <modName> MN </modName>
         <fName>   FN </fName>
         <params>  PARAMS </params>
         ...
       </function>

  rule #allocBind(N, MN:ModName, FN:Name, ARITY, FVS)
    => (N = call #mkGN("alloc")(3 +Int #length(FVS))
        store #qualNToGlobalName(MN . FN), N
        #mkLN("writePtr") = add 1, N
        store ARITY, #mkLN("writePtr")
        #mkLN("writePtr") = add 1, #mkLN("writePtr") 
        store 0, #mkLN("writePtr") // 0 params currently stored
        #bindEnv(N, FVS)
   ):Instructions

  rule #bindEnv(_ , .VarList) => .Instructions
  rule #bindEnv(LN, FV FVS)
    => (#mkLN("writePtr") = add #mkLN("writePtr"), 1
        store #varToLocalName(FV), #mkLN("writePtr")
        #bindEnv(LN, FVS)
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
                      , LN = call #mkGN("PlutusInternal.dispatch")( #useOperand(T1, #mkLN("appT1", !N))
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
  syntax FunctionDefinition ::= "#staticAppendArg"                     [function]
  rule #staticAppendArg
    => define #mkGN("appendArg") (#mkLN("closure"), #mkLN("param")) {
           #mkLN("func") = load #mkLN("closure")
           #mkLN("readPtr") = add #mkLN("closure"), 1
           #mkLN("arity") = load #mkLN("readPtr")
           #mkLN("readPtr") = add #mkLN("readPtr"), 1
           #mkLN("numParamsStored") = load #mkLN("readPtr")

           #mkLN("spaceNeeded") = add #mkLN("numParamsStored"), 3
           #mkLN("ret") = call #mkGN("alloc")(#mkLN("spaceNeeded"))

           #mkLN("writePtr") = #mkLN("ret") 
           store #mkLN("func"), #mkLN("writePtr")
           #mkLN("writePtr") = add #mkLN("writePtr"), 1 
           store #mkLN("arity"), #mkLN("writePtr")
           #mkLN("writePtr") = add #mkLN("writePtr"), 1 
           #mkLN("numParamsStored") = add #mkLN("numParamsStored"), 1
           store #mkLN("numParamsStored"), #mkLN("writePtr")

           #comment("Copy arguments one at a time")
           #mkLN("numParamsToCopy") = sub #mkLN("numParamsStored"), 1
         #label(#mkIN("while"))
           #mkLN("cond")      = cmp eq #mkLN("numParamsToCopy"), 0
         br #mkLN("cond"), #mkIN("done")

           #mkLN("writePtr") = add #mkLN("writePtr"), 1
           #mkLN("readPtr")  = add #mkLN("readPtr"), 1
           #mkLN("v")        = load #mkLN("readPtr")
           store #mkLN("v"), #mkLN("writePtr")
         br #mkIN("while")

         #label(#mkIN("done"))
           #comment("Add new parameter")
           #mkLN("writePtr") = add #mkLN("writePtr"), 1
           store #mkLN("param"), #mkLN("writePtr")

           ret #mkLN("ret")
           .Instructions .LabeledBlocks
        }

  syntax FunctionDefinition ::= "#staticDispatch"                      [function]
  rule #staticDispatch
    => define #mkGN("PlutusInternal.dispatch") (#mkLN("closure"), #mkLN("param")) {
           #mkLN("func")      = load #mkLN("closure")
           #mkLN("readPtr")   = add #mkLN("closure"), 1
           #mkLN("arity")     = load #mkLN("readPtr")
           #mkLN("readPtr")   = add #mkLN("readPtr"), 1
           #mkLN("numParams") = load #mkLN("readPtr")

           #mkLN("numParamsPlus1") = add #mkLN("numParams"), 1
           #mkLN("notEnoughParams") = cmp ne #mkLN("numParamsPlus1"), #mkLN("arity")
         br #mkLN("notEnoughParams"), #mkIN("notEnoughParams")

           #mkLN("haveEnough1") = cmp eq 1, #mkLN("arity")
         br #mkLN("haveEnough1"), #mkIN("haveEnough1")

           #mkLN("haveEnough2") = cmp eq 2, #mkLN("arity")
         br #mkLN("haveEnough2"), #mkIN("haveEnough2")

           #mkLN("haveEnough3") = cmp eq 3, #mkLN("arity")
         br #mkLN("haveEnough3"), #mkIN("haveEnough3")

         #label(#mkIN("notEnoughParams"))
           #mkLN("ret") = call #mkGN("appendArg") (#mkLN("closure"), #mkLN("param"))
           ret #mkLN("ret")

         #label(#mkIN("haveEnough1"))
           #mkLN("ret") = call #mkLN("func") (#mkLN("param"))
           ret #mkLN("ret")

         #label(#mkIN("haveEnough2"))
           #mkLN("ret") = call #mkGN("PlutusInternal.unfoldClosure2") (#mkLN("closure"), #mkLN("param"))
           ret #mkLN("ret")

         #label(#mkIN("haveEnough3"))
           #mkLN("ret") = call #mkGN("PlutusInternal.unfoldClosure3") (#mkLN("closure"), #mkLN("param"))
           ret #mkLN("ret")

           .Instructions .LabeledBlocks
       }

  syntax FunctionDefinition ::= #staticUnfold(Int)                    [function]
  rule #staticUnfold(ARITY)
    => define #mkGN("PlutusInternal.unfoldClosure", ARITY) (#mkLN("closure"), #mkLN("param")) {
           #concatInsListFct( #mkLN("func") = load #mkLN("closure")
                              #mkLN("readPtr") = add #mkLN("closure"), 2
                              #unfoldClosureBody(1, ARITY)
                            , #mkLN("ret") = call #mkLN("func")(#unfoldClosureParams(1, ARITY))
                              ret #mkLN("ret")
                              .Instructions
                            )
           .LabeledBlocks
       }
  syntax Instructions ::= #unfoldClosureBody(Int, Int)                 [function]
  rule #unfoldClosureBody(N, ARITY) => .Instructions
    when N ==Int ARITY
  rule #unfoldClosureBody(N, ARITY)
    => #mkLN("readPtr") = add #mkLN("readPtr"), 1
       #mkLN("param", N) = load #mkLN("readPtr")
       #unfoldClosureBody(N +Int 1, ARITY)
    when N <Int ARITY

  syntax Operands ::= #unfoldClosureParams(Int, Int)                   [function]
  rule #unfoldClosureParams(ARITY, ARITY) => #mkLN("param"), .Operands
  rule #unfoldClosureParams(    N, ARITY)
    => #mkLN("param", N), #unfoldClosureParams(N +Int 1, ARITY)
    when N <Int ARITY

  // Checks that all functions have been converted
  // Finalization
  // ============

  // Some constructs depend on being able to look up the number or arguments
  // a function takes, etc. This means that moving `<function>`s out of the
  // `<functions>` cell into the `<iele>` cell needs to wait until all functions are
  // completely translated. `#translated?` acts as a barrier preventing this
  // from happening.

  syntax K ::= "#translated?"
  rule <plutus> #translated? => .K ... </plutus>
       <totalTranslated> TF </totalTranslated>
       <totalFunctions> TF </totalFunctions>

  rule <function>
          <k> _:Instructions </k>
          <hasDispatchEntry> _:Bool                  </hasDispatchEntry>
          <readyForDispatch> false:Bool => true:Bool </readyForDispatch>
          ...
       </function>
       <totalTranslated> TR => TR +Int 1 </totalTranslated>

  // Convert functions to IELE

  syntax K ::= "#functionsToIele"

  // TODO: streamline converting public and private functions to IELE
  // Convert public `<function>`s to IELE syntax
  rule <plutus> #functionsToIele ... </plutus>
       <functions>
         <function>
           <modName> MN:ModName      </modName>
           <fName>   NAME:Name       </fName>
           <params>  VARS:VarList    </params>
           <k>       IS:Instructions </k>
           ...
         </function> => .Bag
         ...
       </functions>
       <iele> TLDS:TopLevelDefinitions
           => (define public #mkGN(#plToString(MN) +String "." +String #plToString(NAME))
                (#varsToLocalNames(VARS)) { IS .LabeledBlocks }):FunctionDefinition TLDS
       </iele>
       <exports> NL </exports>
       when #in(NAME, NL)

  // Convert private `<function>`s to IELE syntax
  rule <plutus> #functionsToIele ... </plutus>
       <functions>
         <function>
           <modName> MN:ModName      </modName>
           <fName>   NAME:Name       </fName>
           <params>  VARS:VarList    </params>
           <k>       IS:Instructions </k>
           ...
         </function> => .Bag
         ...
       </functions>
       <iele> TLDS:TopLevelDefinitions
           => (define #qualNToGlobalName(MN . NAME)
                (#varsToLocalNames(VARS)) { IS .LabeledBlocks }):FunctionDefinition TLDS
       </iele>
       <exports> NL </exports>
       when notBool #in(NAME, NL)

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
           => define #qualCToGlobalName(QC) ( #createArgs(0, ARITY) ) {
                IS
                .LabeledBlocks
              }
              TLDS
       </iele>

  syntax LocalNames ::= #createArgs(Int, Int)                          [function]
  rule #createArgs(T, T) => .LocalNames
  rule #createArgs(I, T) => #mkLN("param" +String Int2String(I+Int 1)), #createArgs(I +Int 1, T)
                                                                          [owise]

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

  // ... desugar multi-argument application/lambdas
  // ----------------------------------------------

  syntax Tm ::= #desugarApp(Tm)                                        [function]
  rule #desugarApp([F T1:Tm]) => [F T1]
  rule #desugarApp([F T1 T2 TS:TmList]) => #desugarApp([[F T1] T2 TS])

  rule #useOperand([F T1 T2 TS:TmList], LN) => #useOperand(#desugarApp([F T1 T2 TS]), LN)
  rule #loadOperand([F T1 T2 TS:TmList], LN) => #loadOperand(#desugarApp([F T1 T2 TS]), LN)
  rule #bindVar([F T1 T2 TS:TmList], V, LN) => #bindVar(#desugarApp([F T1 T2 TS]), V, LN)
  
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
  syntax LocalNames ::= #mkLNs(Int, String)                            [function]
  syntax IeleName   ::= #mkIN(String)                                  [function]
                      | #mkIN(String, Int)            [function, klabel(mkIN_SI)]
  syntax Name       ::= #mkPN(String)                                  [function]
                      | #mkPN(String, Int)            [function, klabel(mkPN_SI)]
  syntax ModName    ::= #mkMN(String)                                  [function]
  syntax Var        ::= #mkVar(String)                                 [function]
  syntax QualC      ::= #mkQualC(String, String)                       [function]
  // ----------------------------------------------------------------------------
  rule #mkGN(S)    => #parseToken("GlobalName", "@" +String S)
  rule #mkLN(S)    => #parseToken("LocalName",  "%" +String S)
  rule #mkIN(S)    => #parseToken("IeleName",               S)
  rule #mkPN(S)    => #parseToken("Name",                   S)
  rule #mkMN(S)    => #parseToken("ModName",                S)
  rule #mkVar(S)   => #parseToken("Var",                    S)

  rule #mkGN(S, I) => #mkGN(S +String Int2String(I))
  rule #mkLN(S, I) => #mkLN(S +String Int2String(I))
  rule #mkIN(S, I) => #mkIN(S +String Int2String(I))
  rule #mkPN(S, I) => #mkPN(S +String Int2String(I))

  rule #mkLNs(0,   PREFIX) => .LocalNames
  rule #mkLNs(NUM, PREFIX) => #mkLN(PREFIX, !I), #mkLNs(NUM -Int 1, PREFIX)
    when NUM =/=K 0

  // K does not let us make a qualC in one fail swoop using #parseToken(..) . #parseToken(..).
  // Instead, we need to use #parseToken and hold the information needed to make a QualC
  // and pass it to an external #qualCHolder function which puts it together.
  syntax QualC ::= #qualCHolder(K, K)                                  [function]
  rule #mkQualC(MN, C) => #qualCHolder(#parseToken("UpperName", MN), #parseToken("UpperName", C))
  rule #qualCHolder(MN, C) => (MN . C):QualC

  // Converting between sorts
  // ------------------------

  syntax GlobalName ::= #nameToGlobalName(Name)                        [function]
                      | #qualCToGlobalName(QualC)                      [function]
                      | #qualNToGlobalName(QualN)                      [function]
  rule #nameToGlobalName(N:Name) => #mkGN(#plToString(N))
  rule #qualCToGlobalName(MN . C)
    => #mkGN(#plToString(MN) +String "." +String #plToString(C))
  rule #qualNToGlobalName(MN . C)
    => #mkGN(#plToString(MN) +String "." +String #plToString(C))
 
  syntax LocalName ::= #varToLocalName(Var)                            [function]
  rule #varToLocalName(V:Var) => #mkLN(#varToString(V))

  syntax LocalNames ::= #varsToLocalNames(VarList)                     [function]
  rule #varsToLocalNames(.VarList) => .LocalNames
  rule #varsToLocalNames(X XS)     => #varToLocalName(X) , #varsToLocalNames(XS)

  // rule #plToString(X:BinaryBuiltin) => "XXXXXXXX"

  syntax String ::= #plToString(K)          [function, hook(STRING.token2string)]
                  | #varToString(Var)       [function, hook(STRING.token2string)]
                  | #globalNameToString(GlobalName)
                                            [function, hook(STRING.token2string)]

  syntax TmList ::= #valListToTmList(ValList)                          [function]
  rule #valListToTmList(V VL)     => V #valListToTmList(VL)
  rule #valListToTmList(.ValList) => .TmList

  // List Utilities
  // --------------

  syntax VarList ::= #reverse(VarList)         [function, klabel(reverseVarList)]
  syntax VarList ::= #reverseAux(VarList, VarList)
                                            [function, klabel(reverseVarListAux)]
  // ----------------------------------------------------------------------------
  rule #reverse(VL) => #reverseAux(.VarList, VL)
  rule #reverseAux(OUT, .VarList) => OUT
  rule #reverseAux(OUT, V REM) => #reverseAux(V OUT, REM)

  syntax Int ::= #length(VarList)               [function, klabel(lengthVarList)]
               | #length(TyList)                 [function, klabel(lengthTyList)]
               | #length(TmList)                 [function, klabel(lengthTmList)]
  // ----------------------------------------------------------------------------
  rule #length(V VL:VarList) => 1 +Int #length(VL)    rule #length(.VarList) => 0
  rule #length(T TL:TmList ) => 1 +Int #length(TL)    rule #length(.TmList ) => 0
  rule #length(T TL:TyList ) => 1 +Int #length(TL)    rule #length(.TyList ) => 0

  syntax Bool ::= #in(Name, NameList)                                  [function]
  // ----------------------------------------------------------------------------
  rule #in(NAME, .NameList) => false
  rule #in(NAME, NAME  NL) => true
  rule #in(NAME, NAME' NL) => #in(NAME, NL)
    when NAME =/=K NAME'

endmodule
