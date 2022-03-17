# UPLC Polymorphic builtins

```k
require "uplc-configuration.md"

module UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-CONFIGURATION
```

## `ifThenElse`

```k
  rule <k> (builtin ifThenElse) ~> Force => < builtin ifThenElse .List 3 > ... </k>

  rule <k> < builtin ifThenElse
                     (ListItem(< con bool True >)
                      ListItem(V1:Value)
                      ListItem(_)) 0 > => V1 ... </k>

  rule <k> < builtin ifThenElse
                     (ListItem(< con bool False >)
                      ListItem(_)
                      ListItem(V2:Value)) 0 > => V2 ... </k>

  rule <k> < builtin ifThenElse _ 0 > => (error) ... </k> [owise]
```

## `chooseUnit`

```k
  rule <k> (builtin chooseUnit) ~> Force => < builtin chooseUnit .List 2 > ... </k>

  rule <k> < builtin chooseUnit
                     (ListItem(< con unit () >)
                      ListItem(V:Value)) 0 > => V ... </k>
```

## `chooseData`

```k
  rule <k> (builtin chooseData) ~> Force => < builtin chooseData .List 6 > ... </k>

  rule <k> < builtin chooseData
                     (ListItem(< con data { Constr _Ii:Int [ _DL:DataList ] } >)
                      ListItem(C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value)) 0 > => C ... </k>

  rule <k> < builtin chooseData
                     (ListItem(< con data { Map [ _DL:DataPairList ] } >)
                      ListItem(_C:Value)
                      ListItem(M:Value)
                      ListItem(_L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value)) 0 > => M ... </k>

  rule <k> < builtin chooseData
                     (ListItem(< con data { List [ _DL:DataList ] } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value)) 0 > => L ... </k>

  rule <k> < builtin chooseData
                     (ListItem(< con data { Integer _ } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(I:Value)
                      ListItem(_B:Value)) 0 > => I ... </k>

  rule <k> < builtin chooseData
                     (ListItem(< con data { ByteString _ } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(_I:Value)
                      ListItem(B:Value)) 0 > => B ... </k>
```

## `fstPair`

```k
  rule <k> (builtin fstPair) ~> Force => < builtin fstPair .List 1 >  ... </k>

  rule <k> < builtin fstPair
              ListItem(< con pair (T1:TypeConstant) (_T2:TypeConstant)
                         (C1:Constant, _C2:Constant) >) 0 > => < con T1 C1 > ... </k>
```

## `sndPair`

```k
  rule <k> (builtin sndPair) ~> Force => < builtin sndPair .List 1 >  ... </k>

  rule <k> < builtin sndPair
              ListItem(< con pair (_T1:TypeConstant) (T2:TypeConstant)
                         (_C1:Constant, C2:Constant) >) 0 > => < con T2 C2 > ... </k>
```

## `chooseList`

```k
  rule <k> (builtin chooseList) ~> Force => < builtin chooseList .List 3 > ... </k>

  rule <k> < builtin chooseList
                     (ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >)
                      ListItem(V1:Value)
                      ListItem(_V2:Value)) 0 > => V1 ... </k>

  rule <k> < builtin chooseList
                     (ListItem(< con list(_T:TypeConstant) [ _L:ConstantList ] >)
                      ListItem(_V1:Value)
                      ListItem(V2:Value)) 0 > => V2 ... </k> [owise]
```

## `mkCons`

```k
  rule <k> (builtin mkCons) ~> Force => < builtin mkCons .List 2 > ... </k>

  rule <k> < builtin mkCons
              (ListItem(< con T:TypeConstant C:Constant >)
               ListItem(< con list(T) [ L:ConstantList ] >)) 0 > =>
           < con list(T) [ C , L ] > ... </k>
```

## `headList`

```k
  rule <k> (builtin headList) ~> Force => < builtin headList .List 1 > ... </k>

  rule <k> < builtin headList
              ListItem(< con list(T:TypeConstant)
                                 [ C:Constant  , _L:ConstantList ] >) 0 > =>
           (con T C) ... </k>

  rule <k> < builtin headList _ 0 > => (error) ... </k> [owise]

```

## `tailList`

```k
  rule <k> (builtin tailList) ~> Force => < builtin tailList .List 1 > ... </k>

  rule <k> < builtin tailList
              ListItem(< con list(T:TypeConstant) [ .ConstantList ] >) 0 > =>
           < con T [ .ConstantList ] > ... </k>

  rule <k> < builtin tailList
              ListItem(< con list(T:TypeConstant) [ _C:Constant , L:ConstantList ] >) 0 > =>
           < con T [ L ] > ... </k>
```

## `nullList`

```k
  rule <k> (builtin nullList) ~> Force => < builtin nullList .List 1 > ... </k>

  rule <k> < builtin nullList
              ListItem(< con list(_T:TypeConstant) [ .ConstantList ] >) 0 > =>
           < con bool True > ... </k>

  rule <k> < builtin nullList _ 0 > => < con bool False > ... </k> [owise]
```

## `trace`

```k
  rule <k> (builtin trace) ~> Force => < builtin trace .List 2 > ... </k>

  rule <k> < builtin trace
              (ListItem(< con string S >)
               ListItem(V:Value)) 0 > => V ... </k>
       <trace> ... (.List => ListItem(S)) </trace>
```

```k
endmodule
``` 
