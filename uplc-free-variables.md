# UPLC Free variables

```k
require "uplc-syntax.md"
require "uplc-abstract-environment.md"

module UPLC-FREE-VARIABLES
  imports SET
  imports UPLC-SYNTAX
  imports UPLC-ABSTRACT-ENVIRONMENT

  syntax Set ::= #FV(Term) [function, functional, memo]
```

The definition of `FV(_:UplcId)` depends on the structure of the
environment and therefore is done at the level of the instances of the
abstract environment in `UPLC-ENVIRONMENT`.

```
  rule #FV([ T TL ]) => #FV(T) |Set #FVL(TL)
  rule #FV((lam X:UplcId T)) => #FV(E, T) -Set SetItem(X)
  rule #FV((delay T)) => #FV(T)
  rule #FV((force T)) => #FV(T)
  rule #FV(_) => .Set [owise]

  syntax Set ::= #FVL(TermList) [function, functional]

  rule #FVL(T:Term) => #FV(T)
  rule #FVL(T:Term TL:TermList) => #FV(T) |Set #FVL(TL)
```

## Closed terms

```k
  syntax Bool ::= #closed(Term) [function, functional]
  rule #closed(Term) => #FV(Term) ==K .Set
endmodule
```
