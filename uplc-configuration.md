# UPLC configuration

```k
require "domains.md"
require "uplc-syntax.md"
require "uplc-environment.md"

module UPLC-CONFIGURATION
  imports INT
  imports MAP
  imports LIST
  imports UPLC-SYNTAX
  imports UPLC-ENVIRONMENT

  syntax Frame ::= "Force"
                 | "[_" Term Env "]"
                 | "[" Value "_]"
```

## Semantic components

Each semantic component is represented by a K cell. Cell `<k>` is for
the program syntax, `<env>` (a stack of bindings between identifiers
and values) is for its environment, and `<trace>` (a list of strings)
is used to keep track of the data emitted by the `trace` builtin.

```k 

  syntax Program ::= #handleProgram(Program) [function]
                   | Bytes

  rule #handleProgram(C:ConcreteProgram) => C
  rule #handleProgram(F:FlatProgram) => String2Bytes(trimByteString({F}:>ByteString))

  configuration <k> #handleProgram($PGM:Program) </k>
                <env> .Env </env>
                <trace> .List </trace>
```

```k 
endmodule
```
