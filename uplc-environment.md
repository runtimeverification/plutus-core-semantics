# UPLC Environment

```k
require "domains.md"
require "uplc-syntax.md"
require "uplc-abstract-environment.md"
```

```symbolic
require "uplc-genvironment-instance.md"
```

```k
module UPLC-MAP
  imports MAP
  imports MAP-SYMBOLIC
endmodule

module UPLC-ENVIRONMENT
  imports UPLC-ID
  imports BOOL
  imports INT-SYNTAX
  imports UPLC-MAP
  imports UPLC-SYNTAX
  imports LIST
  imports K-EQUAL
  imports UPLC-ABSTRACT-ENVIRONMENT
```

```symbolic
  imports UPLC-GENVIRONMENT-INSTANCE
```

```concrete
  rule #lookup(E:Map, X:UplcId) => { E[X] }:>Value

  rule #def(RHO, X) => X in_keys(RHO)
```

```symbolic
  rule #lookup(_:Map, X:UplcId) => gLookup(X)
  requires #inKeysgEnv(X)

  rule #lookup(E:Map, X:UplcId) => { E[X] }:>Value
  requires notBool #inKeysgEnv(X)
   andBool X in_keys(E)

  rule #def(RHO, X) => X in_keys(RHO) orBool #inKeysgEnv(X)
```

```k
endmodule
```
