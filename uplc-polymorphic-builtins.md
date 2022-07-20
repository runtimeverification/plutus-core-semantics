# UPLC Polymorphic builtins

```k
require "uplc-configuration.md"
require "uplc-syntax.md"

module UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-SYNTAX
  imports BOOL
  imports K-EQUAL
```

## General error rule for polymorphic builtins

All polymorphic builtins should be arguments to a call to `force`.

```k
  rule <k> (builtin _BN:PolyBuiltinName) ~> KI:KItem ~> _ => (error) </k>
  requires KI =/=K Force
```

## `ifThenElse`

```k
  rule #typeSignature(ifThenElse) => ListItem(forall.a*) ListItem(bool) ListItem(a*) ListItem(a*)

  rule <k> #eval(ifThenElse,
                   (ListItem(< con bool True >)
                    ListItem(V1:Value)
                    ListItem(_))) => V1 ... </k>

  rule <k> #eval(ifThenElse,
                   (ListItem(< con bool False >)
                    ListItem(_)
                    ListItem(V2:Value))) => V2 ... </k>
```

## `chooseUnit`

```k
  rule #typeSignature(chooseUnit) => ListItem(forall.a*) ListItem(unit) ListItem(a*)

  rule <k> #eval(chooseUnit,
                   (ListItem(< con unit () >)
                    ListItem(V:Value))) => V ... </k>
```

## `fstPair`

For the moment, we only type check a pair againts `fstPair`'
signature, that is, if it has builtin types for its projections. We
might as well simply return `true` now for `fstPair` as `#eval` would
return an error if `fstPair`'s argument is not a pair.

```k
  rule #typeSignature(fstPair) => ListItem(forall.a#) ListItem(forall.b#) ListItem(pairTV(a#, b#))

  rule <k> #eval(fstPair,
              ListItem(< con pair (T1:TypeConstant) (_T2:TypeConstant)
                         (C1:Constant, _C2:Constant) >)) => < con T1 C1 > ... </k>
```

## `sndPair`

For the moment, we only type check a pair againts `sndPair`' signature,
that is, if it has builtin types for its projections. We
might as well simply return `true` now for `sndPair` as `#eval` would
return an error if `sndPair`'s argument is not a pair.

```k
  rule #typeSignature(sndPair) => ListItem(forall.a#) ListItem(forall.b#) ListItem(pairTV(a#, b#))

  rule <k> #eval(sndPair,
              ListItem(< con pair (_T1:TypeConstant) (T2:TypeConstant)
                         (_C1:Constant, C2:Constant) >)) => < con T2 C2 > ... </k>
```

## `chooseList`

```k
  rule #typeSignature(chooseList) => ListItem(forall.a#) ListItem(forall.b*) ListItem(listTV(a#)) ListItem(b*) ListItem(b*)

  rule <k> #eval(chooseList,
                     (ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >)
                      ListItem(V1:Value)
                      ListItem(_V2:Value))) => V1 ... </k>

  rule <k> #eval(chooseList,
                     (ListItem(< con list(_T:TypeConstant) [ L:ConstantList ] >)
                      ListItem(_V1:Value)
                      ListItem(V2:Value))) => V2 ... </k>
  requires L =/=K .ConstantList
```

## `mkCons`

```k
  rule #typeSignature(mkCons) => ListItem(forall.a#) ListItem(a#) ListItem(listTV(a#))

  rule <k> #eval(mkCons,
              (ListItem(< con T:TypeConstant C:Constant >)
               ListItem(< con list(T) [ L:ConstantList ] >))) =>
           < con list(T) [ C , L ] > ... </k>
```

## `headList`

```k
  rule #typeSignature(headList) => ListItem(forall.a#) ListItem(listTV(a#))

  rule <k> #eval(headList,
              ListItem(< con list(T:TypeConstant)
                                 [ C:Constant  , _L:ConstantList ] >)) =>
           (con T C) ... </k>
```

## `tailList`

```k
  rule #typeSignature(tailList) => ListItem(forall.a#) ListItem(listTV(a#))

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ .ConstantList ] >)) =>
           < con list(T) [ .ConstantList ] > ... </k>

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ _C:Constant , L:ConstantList ] >)) =>
           < con list(T) [ L ] > ... </k>
```

## `nullList`

```k
  rule #typeSignature(nullList) => ListItem(forall.a#) ListItem(listTV(a#))

  rule <k> #eval(nullList,
              ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >)) =>
           < con bool True > ... </k>

  rule <k> #eval(nullList,
              ListItem(< con list(_T:TypeConstant) [ L:ConstantList ] >)) =>
           < con bool False > ... </k>
  requires L =/=K .ConstantList
```

## `trace`

```k
  rule #typeSignature(trace) => ListItem(forall.a*) ListItem(string) ListItem(a*)

  rule <k> #eval(trace,
              (ListItem(< con string S >)
               ListItem(V:Value))) => V ... </k>
       <trace> ... (.List => ListItem(S)) </trace>
```

```k
endmodule
```
