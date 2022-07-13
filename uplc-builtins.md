# UPLC Builtins

```k
require "uplc-polymorphic-builtins.md"
require "uplc-integer-builtins.md"
require "uplc-bytestring-builtins.md"
require "uplc-crypto-builtins.md"
require "uplc-string-builtins.md"
require "uplc-data-builtins.md"

module UPLC-BUILTINS
  imports UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-INTEGER-BUILTINS
  imports UPLC-BYTESTRING-BUILTINS
  imports UPLC-CRYPTO-BUILTINS
  imports UPLC-STRING-BUILTINS
  imports UPLC-DATA-BUILTINS

  rule <k> (builtin BN) => < builtin BN .List #numArgs(BN) > ... </k>
       <env> _ => .Map </env>
    requires notBool isPolyBuiltinName(BN)
     andBool notBool BN ==K chooseData

  rule <k> (builtin (_::PolyBuiltinName #Or chooseData) #as BN) ~> Force => < builtin BN .List #numArgs(BN) > ... </k>
       <env> _ => .Map </env>

endmodule
```
