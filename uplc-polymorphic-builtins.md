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
  rule #numArgs(ifThenElse) => 3

  rule #typeCheck(ListItem(< con bool _ >), ifThenElse, 1) => true

  rule #typeCheck(_, ifThenElse, 2) => true

  rule #typeCheck(ListItem(< con bool _ >) ListItem(_:Value) ListItem(_:Value) ,
         ifThenElse, 3) => true

  rule <k> (builtin ifThenElse) ~> Force => < builtin ifThenElse .List 3 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

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
  rule #numArgs(chooseUnit) => 2

  rule #typeCheck(ListItem(< con unit () >) _, chooseUnit, 1) => true

  rule #typeCheck(ListItem(< con unit () >) ListItem(_:Value) .List, chooseUnit, 2) => true

  rule <k> (builtin chooseUnit) ~> Force => < builtin chooseUnit .List 2 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

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
  rule #numArgs(fstPair) => 1

  rule #typeCheck(ListItem(< con pair (_:TypeConstant) (_:TypeConstant) (_,_) >),
                             fstPair, 1) => true

  rule <k> (builtin fstPair) ~> Force => < builtin fstPair .List 1 >  ... </k>
       <currentEnv> _ => 0 </currentEnv>

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
  rule #numArgs(sndPair) => 1

  rule #typeCheck(ListItem(< con pair (_:TypeConstant) (_:TypeConstant) (_,_) >),
                             sndPair, 1) => true

  rule <k> (builtin sndPair) ~> Force => < builtin sndPair .List 1 >  ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> #eval(sndPair,
              ListItem(< con pair (_T1:TypeConstant) (T2:TypeConstant)
                         (_C1:Constant, C2:Constant) >)) => < con T2 C2 > ... </k>
```

## `chooseList`

```k
  rule #numArgs(chooseList) => 3

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _ ] >), chooseList, 1) => true

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _ ] >)
                  ListItem(_:Value),
                  chooseList, 2) => true

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _ ] >)
                  ListItem(_:Value)
                  ListItem(_:Value),
                  chooseList, 3) => true

  rule <k> (builtin chooseList) ~> Force => < builtin chooseList .List 3 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

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
  rule #numArgs(mkCons) => 2

  rule #typeCheck(ListItem(< con _:TypeConstant _:Constant >), mkCons, 1) => true

  rule #typeCheck(ListItem(< con T:TypeConstant _:Constant >)
                  ListItem(< con list(T) [ _:ConstantList ] >), mkCons, 2) => true

  rule <k> (builtin mkCons) ~> Force => < builtin mkCons .List 2 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> #eval(mkCons,
              (ListItem(< con T:TypeConstant C:Constant >)
               ListItem(< con list(T) [ L:ConstantList ] >))) =>
           < con list(T) [ C , L ] > ... </k>
```

## `headList`

```k
  rule #numArgs(headList) => 1

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _:ConstantList ] >), headList, 1) => true

  rule <k> (builtin headList) ~> Force => < builtin headList .List 1 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> #eval(headList,
              ListItem(< con list(T:TypeConstant)
                                 [ C:Constant  , _L:ConstantList ] >)) =>
           (con T C) ... </k>
```

## `tailList`

```k
  rule #numArgs(tailList) => 1

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _:ConstantList ] >), tailList, 1) => true

  rule <k> (builtin tailList) ~> Force => < builtin tailList .List 1 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ .ConstantList ] >)) =>
           < con list(T) [ .ConstantList ] > ... </k>

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ _C:Constant , L:ConstantList ] >)) =>
           < con list(T) [ L ] > ... </k>
```

## `nullList`

```k
  rule #numArgs(nullList) => 1

  rule #typeCheck(ListItem(< con list(_:TypeConstant) [ _:ConstantList ] >), nullList, 1) => true

  rule <k> (builtin nullList) ~> Force => < builtin nullList .List 1 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

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
  rule #numArgs(trace) => 2

  rule #typeCheck(ListItem(< con string _ >), trace, 1) => true

  rule #typeCheck(ListItem(< con string _ >)
                  ListItem(_:Value), trace, 2) => true

  rule <k> (builtin trace) ~> Force => < builtin trace .List 2 > ... </k>
       <currentEnv> _ => 0 </currentEnv>

  rule <k> #eval(trace,
              (ListItem(< con string S >)
               ListItem(V:Value))) => V ... </k>
       <trace> ... (.List => ListItem(S)) </trace>
```

```k
endmodule
```
