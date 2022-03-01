# UPLC configuration

```k
require "domains.md"
require "uplc-syntax.md"

module UPLC-CONFIGURATION
  imports UPLC-SYNTAX
  imports INT
  imports MAP
  imports LIST

  syntax AClosure ::= Clos(Value, Map)

  syntax ATerm ::= "Force"
                 | "[_" Term "]"
                 | "[" AClosure "_]"
                 | Term
```

## Semantic components

Each semantic component is represented by a K cell. Cell `<k>` is for
the program syntax, `<env>` is for its environment, `<args>` is used
to evaluate builtin arguments when the uncurried style us used, and
`<trace>` is used to keep track of the data emitted by the `trace`
builtin.

```k 
  configuration <k> #handleProgram($PGM:Program) </k>
                <env> .Map </env>
                <trace> .List </trace>
```

```k 
endmodule
```
