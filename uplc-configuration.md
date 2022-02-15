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

  configuration <k> $PGM:Program </k>
                <env> .Map </env>
                <stack> .List </stack>
endmodule
``` 