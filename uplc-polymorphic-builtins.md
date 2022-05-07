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
  rule <k> (builtin BN:PolyBuiltinName) ~> K:KItem => (error) ... </k>
  requires K =/=K Force
```

## `ifThenElse`

```k
  rule <k> (builtin ifThenElse) ~> Force => < builtin ifThenElse .List 3 > ... </k>

  rule <k> #eval(ifThenElse,
                   (ListItem(< con bool True >)
                    ListItem(V1:Value)
                    ListItem(_))) => V1 ... </k>

  rule <k> #eval(ifThenElse,
                   (ListItem(< con bool False >)
                    ListItem(_)
                    ListItem(V2:Value))) => V2 ... </k>

  rule <k> #eval(ifThenElse, _) => (error) ... </k> [owise]
```

## `chooseUnit`

```k
  rule <k> (builtin chooseUnit) ~> Force => < builtin chooseUnit .List 2 > ... </k>

  rule <k> #eval(chooseUnit,
                   (ListItem(< con unit () >)
                    ListItem(V:Value))) => V ... </k>

  rule <k> #eval(chooseUnit, _) => (error) ... </k> [owise]
```

## `fstPair`

```k
  rule <k> (builtin fstPair) ~> Force => < builtin fstPair .List 1 >  ... </k>

  rule <k> #eval(fstPair,
              ListItem(< con pair (T1:TypeConstant) (_T2:TypeConstant)
                         (C1:Constant, _C2:Constant) >)) => < con T1 C1 > ... </k>

  rule <k> #eval(fstPair, _) => (error) ... </k> [owise]
```

## `sndPair`

```k
  rule <k> (builtin sndPair) ~> Force => < builtin sndPair .List 1 >  ... </k>

  rule <k> #eval(sndPair,
              ListItem(< con pair (_T1:TypeConstant) (T2:TypeConstant)
                         (_C1:Constant, C2:Constant) >)) => < con T2 C2 > ... </k>

  rule <k> #eval(sndPair, _) => (error) ... </k> [owise]
```

## `chooseList`

```k
  rule <k> (builtin chooseList) ~> Force => < builtin chooseList .List 3 > ... </k>

  rule <k> #eval(chooseList,
                     (ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >)
                      ListItem(V1:Value)
                      ListItem(_V2:Value))) => V1 ... </k>

  rule <k> #eval(chooseList,
                     (ListItem(< con list(_T:TypeConstant) [ L:ConstantList ] >)
                      ListItem(_V1:Value)
                      ListItem(V2:Value))) => V2 ... </k>
  requires L =/=K .ConstantList

  rule <k> #eval(chooseList, _) => (error) ... </k> [owise]
```

## `mkCons`

```k
  rule <k> (builtin mkCons) ~> Force => < builtin mkCons .List 2 > ... </k>

  rule <k> #eval(mkCons,
              (ListItem(< con T:TypeConstant C:Constant >)
               ListItem(< con list(T) [ L:ConstantList ] >))) =>
           < con list(T) [ C , L ] > ... </k>

  rule <k> #eval(mkCons, _) => (error) ... </k> [owise]
```

## `headList`

```k
  rule <k> (builtin headList) ~> Force => < builtin headList .List 1 > ... </k>

  rule <k> #eval(headList,
              ListItem(< con list(T:TypeConstant)
                                 [ C:Constant  , _L:ConstantList ] >)) =>
           (con T C) ... </k>

  rule <k> #eval(headList, _) => (error) ... </k> [owise]

```

## `tailList`

```k
  rule <k> (builtin tailList) ~> Force => < builtin tailList .List 1 > ... </k>

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ .ConstantList ] >)) =>
           < con T [ .ConstantList ] > ... </k>

  rule <k> #eval(tailList,
              ListItem(< con list(T:TypeConstant) [ _C:Constant , L:ConstantList ] >)) =>
           < con T [ L ] > ... </k>

  rule <k> #eval(tailList, _) => (error) ... </k> [owise]
```

## `nullList`

```k
  rule <k> (builtin nullList) ~> Force => < builtin nullList .List 1 > ... </k>

  rule <k> #eval(nullList,
              ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >)) =>
           < con bool True > ... </k>

  rule <k> #eval(nullList,
              ListItem(< con list(_T:TypeConstant) [ L:ConstantList ] >)) =>
           < con bool False > ... </k>
  requires L =/=K .ConstantList 

  rule <k> #eval(nullList, _) => (error) ... </k> [owise]
```

## `trace`

```k
  rule <k> (builtin trace) ~> Force => < builtin trace .List 2 > ... </k>

  rule <k> #eval(trace, 
              (ListItem(< con string S >)
               ListItem(V:Value))) => V ... </k>
       <trace> ... (.List => ListItem(S)) </trace>

  rule <k> #eval(trace, _) => (error) ... </k> [owise]
```

```k
endmodule
``` 
