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

  syntax Bool ::= Value "~" TypeVariable [function, klabel(typeCompatible), symbol]
  rule < con A _ > ~ A                         => true           // $\iota \in U$ and $V \in C_{\iota}$
  rule _:Value     ~ _:PolyBuiltinTypeVariable => true           // $\iota \in V_{#}$
  rule _:Value     ~ listTV(_)                 => true           // $\iota \in V_{#}$
  rule _:Value     ~ pairTV(_,_)               => true           // $\iota \in V_{#}$
  rule _:Value     ~ _:FullyPolyTypeVariable   => true           // $\iota \in V_{*}$
  rule _:Value     ~ _                         => false [owise]

endmodule
```
