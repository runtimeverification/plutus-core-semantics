# UPLC configuration

```k
require "domains.md"
require "uplc-syntax.md"
require "uplc-environment.md"
require "uplc-flat-parser.md"

module UPLC-CONFIGURATION
  imports INT
  imports MAP
  imports LIST
  imports UPLC-SYNTAX
  imports UPLC-FLAT-PARSER
  imports UPLC-SYNTAX
  imports UPLC-ENVIRONMENT

  syntax Frame ::= "Force"
                 | "[_" Term Map "]"
                 | "[" Value "_]"
```

## Semantic components

Each semantic component is represented by a K cell. Cell `<k>` is for
the program syntax, `<env>` (a stack of bindings between identifiers
and values) is for its environment, and `<trace>` (a list of strings)
is used to keep track of the data emitted by the `trace` builtin.

```k
  syntax UValue ::= Value
  syntax UValue ::= #eval(BuiltinName, List)
  rule <k> #eval(_,_) ~> _ => (error) </k> [owise]

  syntax Int  ::= #numArgs(BuiltinName) [function]
  syntax Bool ::= #typeCheck(List, BuiltinName, Int) [function]
  rule #typeCheck(_,_,_) => false [owise]

  syntax Program ::= #handleProgram(Program) [function]

  rule #handleProgram(C:ConcreteProgram) => C
  rule #handleProgram(F:FlatProgram) => #bytes2program(getBytes(F))

  syntax Bytes ::= getBytes(FlatProgram) [function]
  rule getBytes(F) =>
    #let
      INPUT=trimByteString({F}:>ByteString)
    #in
      Int2Bytes( lengthString( INPUT ) /Int 2, String2Base( INPUT, 16 ), BE)

  configuration <k> #handleProgram($PGM:Program) </k>
                <env> .Map </env>
                <heap> .Map </heap>
                <trace> .List </trace>
```

```k
endmodule
```
