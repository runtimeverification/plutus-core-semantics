

















module IELE-SYNTAX
  imports IELE-COMMON

  syntax IeleName ::= r"(?<![A-Za-z0-9\\_\\.\\-\\$])[a-zA-Z\\.\\_\\-\\$][0-9a-zA-Z\\.\\_\\-\\$]*" [token, notInRules, prec(3)]

  syntax NumericIeleName ::= r"[0-9]+" [token]
endmodule





module IELE-COMMON
  imports DOMAINS-SYNTAX
  syntax NumericIeleName ::= Int
  syntax IeleName ::= NumericIeleName










  syntax GlobalName ::= "@" IeleName

  syntax LocalName ::= "%" IeleName

  syntax LocalNames ::= List{LocalName, ","} [klabel(localNameList)]








  syntax Constant ::= Int | GlobalName















  syntax LValue ::= LocalName
  
  syntax LValues ::= NeList{LValue, ","} [klabel(lvalueList)]

  syntax Operand ::= LValue | Constant

  syntax Operands ::= List{Operand, ","} [klabel(operandList), hybrid, strict]

  syntax NonEmptyOperands ::= NeList{Operand, ","} [klabel(operandList)]

  syntax Ints ::= List{Int, ","} [klabel(operandList)]

  syntax Operands ::= Ints







  syntax AssignInst ::= LValue "=" Operand







  syntax LoadInst ::= LValue "=" "load" /* cell */ Operand [hybrid, strict(2)]
  syntax LoadInst ::= LValue "=" "load" /* cell */ Operand "," /* offset in bytes */ Operand "," /* width in bytes */ Operand [hybrid, seqstrict(2,3,4)]
  syntax StoreInst ::= "store" /* value */ Operand "," /* cell */ Operand [hybrid, seqstrict(1,2)]
  syntax StoreInst ::= "store" /* value */ Operand "," /* cell */ Operand "," /* offset in bytes */ Operand "," /* width in bytes */ Operand [hybrid, seqstrict(1,2,3,4)]







  syntax SLoadInst ::= LValue "=" "sload" /* index */ Operand [hybrid, strict(2)]
  syntax SStoreInst ::= "sstore" /* value */ Operand "," /* index */ Operand [hybrid, seqstrict(1,2)]







  syntax IsZeroInst ::= LValue "=" "iszero" Operand [hybrid, strict(2)]
  syntax NotInst    ::= LValue "=" "not"    Operand [hybrid, strict(2)]

  syntax AddInst ::= LValue "=" "add" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax MulInst ::= LValue "=" "mul" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax SubInst ::= LValue "=" "sub" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax DivInst ::= LValue "=" "div" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax ExpInst ::= LValue "=" "exp" Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax ModInst ::= LValue "=" "mod" Operand "," Operand [hybrid, seqstrict(2,3)]

  syntax AddModInst ::= LValue "=" "addmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]
  syntax MulModInst ::= LValue "=" "mulmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]
  syntax ExpModInst ::= LValue "=" "expmod" Operand "," Operand "," Operand [hybrid, seqstrict(2,3,4)]

  syntax ByteInst ::= LValue "=" "byte" /* byte index, 0 being the LSB */ Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax SExtInst ::= LValue "=" "sext" /* width in bytes */              Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax TwosInst ::= LValue "=" "twos" /* width in bytes */              Operand "," Operand [hybrid, seqstrict(2,3)]

  syntax AndInst   ::= LValue "=" "and"   Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax OrInst    ::= LValue "=" "or"    Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax XorInst   ::= LValue "=" "xor"   Operand "," Operand [hybrid, seqstrict(2,3)]
  syntax ShiftInst ::= LValue "=" "shift" Operand "," /* shift amount */ Operand [hybrid, seqstrict(2,3)]

  syntax Predicate ::= "lt" | "le" | "gt" | "ge" | "eq" | "ne"
  syntax CmpInst ::= LValue "=" "cmp" Predicate Operand "," Operand [hybrid, seqstrict(3,4)]

  syntax SHA3Inst ::= LValue "=" "sha3" Operand [hybrid, strict(2)]







  syntax JumpInst     ::= "br" IeleName
  syntax CondJumpInst ::= "br" Operand "," IeleName [hybrid, strict(1)]







  syntax LocalCallInst   ::= "call" GlobalName "(" Operands ")"
                           | LValues "=" "call" GlobalName "(" Operands ")" [hybrid, strict(3)]
  syntax AccountCallInst ::= "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand
                           | LValues "=" "call" GlobalName "at" Operand "(" Operands ")" "send" Operand "," "gaslimit" Operand [hybrid, seqstrict(3,4,5,6)]
  syntax AccountCallInst ::= "staticcall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand
                           | LValues "=" "staticcall" GlobalName "at" Operand "(" Operands ")" "gaslimit" Operand [hybrid, seqstrict(3,4,5)]

  syntax ReturnInst ::= "ret" NonEmptyOperands [hybrid, strict(1)]
                      | "ret" "void"
  syntax RevertInst ::= "revert" Operand [hybrid, strict(1)]







  syntax LogInst ::= "log" /* cell */ Operand [hybrid, strict(1)]
                   | "log" /* cell */ Operand "," NonEmptyOperands [hybrid, seqstrict(1,2)]







  syntax CreateInst ::= /* exit status */ LValue "," /* new account address */ LValue "=" "create"     /* contract name */    IeleName "(" Operands ")" "send" Operand [hybrid, seqstrict(4,5)]
  syntax CreateInst ::= /* exit status */ LValue "," /* new account address */ LValue "=" "copycreate" /* contract address */ Operand  "(" Operands ")" "send" Operand [hybrid, seqstrict(3,4,5)]

  syntax SelfdestructInst ::= "selfdestruct" /* account to send balance */ Operand [hybrid, strict(1)]







  syntax IeleName ::= "iele.invalid"     [token]
  // local state queries
  syntax IeleName ::= "iele.gas"         [token]
  syntax IeleName ::= "iele.gasprice"    [token]
  syntax IeleName ::= "iele.gaslimit"    [token]
  syntax IeleName ::= "iele.beneficiary" [token]
  syntax IeleName ::= "iele.timestamp"   [token]
  syntax IeleName ::= "iele.number"      [token]
  syntax IeleName ::= "iele.difficulty"  [token]
  syntax IeleName ::= "iele.address"     [token]
  syntax IeleName ::= "iele.origin"      [token]
  syntax IeleName ::= "iele.caller"      [token]
  syntax IeleName ::= "iele.callvalue"   [token]
  syntax IeleName ::= "iele.msize"       [token]
  syntax IeleName ::= "iele.codesize"    [token]
  syntax IeleName ::= "iele.blockhash"   [token]
  // account queries
  syntax IeleName ::= "iele.balance"     [token]
  syntax IeleName ::= "iele.extcodesize" [token]







  syntax IeleName ::= "iele.ecrec"     [token]
  syntax IeleName ::= "iele.sha256"    [token]
  syntax IeleName ::= "iele.rip160"    [token]
  syntax IeleName ::= "iele.id"        [token]
  syntax IeleName ::= "iele.ecadd"     [token]
  syntax IeleName ::= "iele.ecmul"     [token]
  syntax IeleName ::= "iele.ecpairing" [token]





  syntax Instruction ::=
    AssignInst
  | LoadInst
  | StoreInst
  | SLoadInst
  | SStoreInst
  | IsZeroInst
  | NotInst
  | AddInst
  | MulInst
  | SubInst
  | DivInst
  | ExpInst
  | ModInst
  | AddModInst
  | MulModInst
  | ExpModInst
  | ByteInst
  | SExtInst
  | TwosInst
  | AndInst
  | OrInst
  | XorInst
  | ShiftInst
  | CmpInst
  | SHA3Inst
  | JumpInst
  | CondJumpInst
  | LocalCallInst
  | AccountCallInst
  | ReturnInst
  | RevertInst
  | LogInst
  | CreateInst
  | SelfdestructInst

  syntax Instructions ::= List{Instruction, ""} [klabel(instructionList), format(%1%2%n%3)]
















  syntax TopLevelDefinition ::=
    FunctionDefinition
  | GlobalDefinition
  | ContractDeclaration

  syntax TopLevelDefinitions ::= List{TopLevelDefinition, ""} [klabel(topLevelDefinitionList), format(%1%2%n%3)]

  syntax ContractDefinition ::= "contract" IeleName "{" TopLevelDefinitions "}"

  syntax Contract ::= List{ContractDefinition, ""} [klabel(contractDefinitionList)]







  syntax ContractDeclaration ::= "external" "contract" IeleName







  syntax GlobalDefinition ::= GlobalName "=" Int







  syntax FunctionSignature ::= GlobalName "(" FunctionParameters ")"

  syntax FunctionParameters ::= LocalNames

  syntax FunctionDefinition ::= 
    "define" FunctionSignature "{" Blocks "}"                   [format(%1 %2 %3%i%n%4%d%n%5%n)]
  | "define" "public" FunctionSignature "{" Blocks "}"







  syntax LabeledBlock ::= IeleName ":" Instructions

  syntax LabeledBlocks ::= List{LabeledBlock, ""} [klabel(labeledBlockList)]

  syntax UnlabeledBlock ::= Instructions

  syntax Blocks [flatPredicate]
  syntax Blocks ::= UnlabeledBlock LabeledBlocks | LabeledBlocks








  syntax IeleName ::= "deposit" [token]

  syntax IeleName ::= "init" [token]







  // rule call NAME ( ARGS ) => .LValues = call NAME ( ARGS ) [macro]
  // rule call NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT => .LValues = call NAME at CONTRACT ( ARGS ) send VALUE , gaslimit GLIMIT [macro]
  // rule staticcall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT => .LValues = staticcall NAME at CONTRACT ( ARGS ) gaslimit GLIMIT [macro]
  // rule ret void => ret .NonEmptyOperands [macro]
endmodule


