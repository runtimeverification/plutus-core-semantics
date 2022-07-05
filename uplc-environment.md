# UPLC Environment

```k
require "uplc-syntax.md"

module UPLC-ENVIRONMENT
  imports ENV-CONCRETE
  imports ENV-SYMBOLIC
endmodule

module ENV-CONCRETE [concrete]
  imports INT
  imports LIST
  imports UPLC-ID
  imports MAP

  syntax Env ::= Map
  syntax Map ::= ".Env" [macro]
  rule .Env => .Map

  syntax Map ::= #push(Env, UplcId, Int) [function]
  rule #push(E:Env, X:UplcId, I:Int) => {E}:>Map[X <- {{E}:>Map[X] orDefault .List}:>List ListItem(I)]

  syntax Int ::= #last(List) [function]
  rule #last(L:List) => {L[-1]}:>Int

  syntax Value ::= #lookup(Map, UplcId, Map) [function]
  rule #lookup(E:Map, X:UplcId, H:Map) => { H[ #last( { E[X] }:>List ) ] }:>Value
  requires X in_keys(E)

endmodule

```

This module implements a separate `#lookup` function. The main difference is the removal of the list cast and the parameter types.

```k
module ENV-SYMBOLIC [symbolic]
  imports ENV-SYMBOLIC-CORE
  imports MAP
  imports MAP-SYMBOLIC

  syntax Int ::= #last(List) [function]
  rule #last(L:List) => {L[-1]}:>Int

  syntax Value ::= #lookup(Env, UplcId, Map) [function]
  rule #lookup(E:Env, X:UplcId, H:Map) => { H[ #last( E[X] ) ] }:>Value
  requires X in_keys(E)

endmodule
```

The Symbolic implementation of Env - a map where keys are UplcId's and the values are lists. Using this implementation makes `#push`
functional. A regular Map would require a cast to List which can result in bottom.

```k
module ENV-SYMBOLIC-CORE [symbolic]
  imports INT
  imports STRING
  imports LIST
  imports SET
  imports UPLC-ID
  imports private K-EQUAL
  imports private BOOL

  syntax Env ::= #push(Env, UplcId, Int) [function, functional]
  rule #push(E:Env, X:UplcId, I:Int) => E[X <- (E[X] orDefault .List) ListItem(I)]

```

The following definitions are boilerplate Map functions from domains.md.

```k

  syntax Env [hook(MAP.Map)]

  syntax Env ::= Env Env
      [ left, function, hook(MAP.concat), klabel(_Env_), symbol, assoc, comm
      , unit(.Env), element(_|->Env_), index(0), format(%1%n%2)
      ]

  syntax Env ::= ".Env"
      [ function, functional, hook(MAP.unit), klabel(.Env), symbol
      , latex(\dotCt{Env})
      ]

  syntax Env ::= UplcId "|->" List
      [ function, functional, hook(MAP.element), klabel(_|->Env_), symbol
      , latex({#1}\mapsto{#2})
      ]

  syntax priorities _|->Env_ > _Env_ .Env

  syntax non-assoc _|->Env_

  syntax List ::= Env "[" UplcId "]"
      [function, hook(MAP.lookup), klabel(Env:lookup), symbol]

  syntax List ::= Env "[" UplcId "]" "orDefault" List [function, functional, hook(MAP.lookupOrDefault), klabel(Env:lookupOrDefault)]

  syntax Env ::= Env "[" UplcId "<-" value:List "]" [function, functional, klabel(Env:update), symbol, hook(MAP.update), prefer]

  syntax Env ::= Env "[" UplcId "<-" "undef" "]" [function, functional, hook(MAP.remove), klabel(Env:removeEntryEnv), symbol]

  syntax Env ::= Env "-Env" Env
      [function, functional, hook(MAP.difference), latex({#1}-_{\it Env}{#2}), klabel(_-Env_)]

  syntax Env ::= updateEnv(Env, Env) [function, functional, hook(MAP.updateAll), klabel(Env:updateAll)]

  syntax Env ::= removeAll(Env, Set) [function, functional, hook(MAP.removeAll), klabel(Env:removeAll)]

  syntax Set ::= keys(Env) [function, functional, hook(MAP.keys), klabel(Env:keys)]

  syntax List ::= "keys_list" "(" Env ")" [function, hook(MAP.keys_list), klabel(Env:keys_list)]

  syntax Bool ::= UplcId "in_keys" "(" Env ")" [function, functional, hook(MAP.in_keys), klabel(Env:in_keys)]

  syntax List ::= values(Env) [function, hook(MAP.values), klabel(Env:values)]

  syntax Int ::= size(Env) [function, functional, hook(MAP.size), klabel(Env:size)]

  syntax Bool ::= Env "<=Env" Env
      [function, functional, hook(MAP.inclusion), klabel(Env:_<=Env_)]

  syntax KItem ::= choice(Env) [function, hook(MAP.choice), klabel(Env:choice)]
```

The following simplification rules are lifted from the MAP-SYMBOLIC module from domains.md.

```k

  rule #Ceil(@E:Env [@U:UplcId]) => {(@U in_keys(@E)) #Equals true} #And #Ceil(@E) #And #Ceil(@U) [anywhere, simplification]

  // Symbolic update

  // Adding the definedness condition `notBool (K in_keys(E))` in the ensures clause of the following rule would be redundant
  // because K also appears in the rhs, preserving the case when it's #Bottom.
  rule (K |-> _ E:Env) [ K <- V ] => (K |-> V E) [simplification]
  rule E:Env [ K <- V ] => (K |-> V E) requires notBool (K in_keys(E)) [simplification]
  rule E:Env [ K <- _ ] [ K <- V ] => E [ K <- V ] [simplification]
  // Adding the definedness condition `notBool (K1 in_keys(E))` in the ensures clause of the following rule would be redundant
  // because K1 also appears in the rhs, preserving the case when it's #Bottom.
  rule (K1 |-> V1 E:Env) [ K2 <- V2 ] => (K1 |-> V1 (E [ K2 <- V2 ])) requires K1 =/=K K2 [simplification]

  // Symbolic remove
  rule (K |-> _ E:Env) [ K <- undef ] => E ensures notBool (K in_keys(E)) [simplification]
  rule E:Env [ K <- undef ] => E requires notBool (K in_keys(E)) [simplification]
  // Adding the definedness condition `notBool (K1 in_keys(E))` in the ensures clause of the following rule would be redundant
  // because K1 also appears in the rhs, preserving the case when it's #Bottom.
  rule (K1 |-> V1 E:Env) [ K2 <- undef ] => (K1 |-> V1 (E [ K2 <- undef ])) requires K1 =/=K K2 [simplification]

  // Symbolic lookup
  rule (K  |->  V E:Env) [ K ]  => V ensures notBool (K in_keys(E)) [simplification]
  rule (K1 |-> _V E:Env) [ K2 ] => E [K2] requires K1 =/=K K2 ensures notBool (K1 in_keys(E)) [simplification]
  rule (_E:Env [ K  <-  V1 ]) [ K ]  => V1 [simplification]
  rule ( E:Env [ K1 <- _V1 ]) [ K2 ] => E [ K2 ] requires K1 =/=K K2 [simplification]

  // Symbolic in_keys
  rule K in_keys(_E [ K <- undef ]) => false [simplification]
  rule K in_keys(_E [ K <- _ ]) => true [simplification]
  rule K1 in_keys(E [ K2 <- _ ]) => true requires K1 ==K K2 orBool K1 in_keys(E) [simplification]
  rule K1 in_keys(E [ K2 <- _ ]) => K1 in_keys(E) requires K1 =/=K K2 [simplification]

/*
// The rule below is automatically generated by the frontend for every sort
// hooked to MAP.Map. It is left here to serve as documentation.

  rule #Ceil(@M:Map (@K:KItem |-> @V:KItem)) => {(@K in_keys(@M)) #Equals false} #And #Ceil(@M) #And #Ceil(@K) #And #Ceil(@V)
    [anywhere, simplification]
*/
```

the above comment seems like a useful rule: this is the implementation for Env with the approperiate types.

```k
  rule #Ceil(@E:Env (@U:UplcId |-> @L:List)) => {(@U in_keys(@E)) #Equals false} #And #Ceil(@E) #And #Ceil(@U) #And #Ceil(@L)
    [anywhere, simplification]

endmodule
```
