# `ENV` module

```k
require "uplc-syntax.md"

module ENV [symbolic]
  imports INT
  imports STRING
  imports LIST
  imports SET
  imports UPLC-ID

  syntax Env [hook(MAP.Map)]

  syntax Env ::= Env Env
      [ left, function, hook(MAP.concat), klabel(_Env_), symbol, assoc, comm
      , unit(.Env), element(_|->Env_), index(0), format(%1%n%2)
      ]

  syntax Env ::= ".Env"
      [ function, functional, hook(MAP.unit), klabel(.Env), symbol
      , latex(\dotCt{Env})
      ]

  syntax Env ::= UplcId "|->Env" List
      [ function, functional, hook(MAP.element), klabel(_|->Env_), symbol
      , latex({#1}\mapsto{#2})
      ]

  syntax priorities _|->Env_ > _Env_ .Env

  syntax non-assoc _|->Env_

  syntax List ::= lookupEnv(Env, UplcId)
      [function, hook(MAP.lookup), klabel(Env:lookup), symbol]

  syntax List ::= lookupOrDefaultEnv(Env, UplcId, KItem)
      [function, functional, hook(MAP.lookupOrDefault), klabel(Env:lookupOrDefault)]

  syntax Env ::= updateEntryEnv(Env, UplcId, List)
      [function, functional, klabel(Env:update), symbol, hook(MAP.update), prefer]

  syntax Env ::= removeEntryEnv(Env, UplcId)
      [function, functional, hook(MAP.remove), klabel(Env:removeEntryEnv), symbol]

  syntax Env ::= Env "-Env" Env
      [function, functional, hook(MAP.difference), latex({#1}-_{\it Env}{#2}), klabel(_-Env_)]

  syntax Env ::= updateAllEnv(Env, Env)
      [function, functional, hook(MAP.updateAll), klabel(Env:updateAllEnv)]

  syntax Env ::= removeAllEnv(Env, Set)
      [function, functional, hook(MAP.removeAll), klabel(Env:removeAllEnv)]

  syntax Set ::= keysEnv(Env)
      [function, functional, hook(MAP.keys), klabel(Env:keysEnv)]

  syntax List ::= "keys_listEnv" "(" Env ")"
      [function, hook(MAP.keys_list), klabel(Env:keys_listEnv)]

  syntax Bool ::= UplcId "in_keysEnv" "(" Env ")"
      [function, functional, hook(MAP.in_keys), klabel(Env:in_keysEnv)]

  syntax List ::= valuesEnv(Env)
      [function, hook(MAP.values), klabel(Env:valuesEnv)]

  syntax Int ::= sizeEnv(Env)
      [function, functional, hook(MAP.size), klabel(Env:sizeEnv)]

  syntax Bool ::= Env "<=Env" Env
      [function, functional, hook(MAP.inclusion), klabel(Env:_<=Env_)]

  syntax KItem ::= choiceEnv(Env)
      [function, hook(MAP.choice), klabel(Env:choiceEnv)]
endmodule
```