# UPLC Polymorphic builtins

```k
require "uplc-configuration.md"

module UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-CONFIGURATION
```

## `ifThenElse`

```k
  rule <k> (builtin ifThenElse) ~> Force => #ITE ... </k>

  rule <k> (V:Value ~> ([ Clos(#ITE, _RHO) _])) => #ITE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ITE(V1:Value), _RHO) _])) => #ITE(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#ITE(V1:Value, V2:Value), _RHO) _])) => #ITE(V1, V2, V3) ... </k>

  rule <k> #ITE((con bool True), V1:Value, _V2:Value) => V1 ... </k>

  rule <k> #ITE((con bool False), _V1:Value, V2:Value) => V2 ... </k>
```

## `chooseUnit`

```k
  rule <k> (builtin chooseUnit) ~> Force => #CUT ... </k>

  rule <k> (V:Value ~> ([ Clos(#CUT, _RHO) _])) => #CUT(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CUT(V1:Value), _RHO) _])) => #CUT(V1, V2) ... </k>

  rule <k> #CUT((con unit ()), V:Value) => V ... </k>
```

## `chooseData`

```k
  rule <k> (builtin chooseData) ~> Force => #CDT ... </k>

  rule <k> (V:Value ~> ([ Clos(#CDT, _RHO) _])) => #CDT(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CDT(V1:Value), _RHO) _])) => #CDT(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#CDT(V1:Value, V2:Value), _RHO) _])) =>
           #CDT(V1, V2, V3) ... </k>

  rule <k> (V4:Value ~> ([ Clos(#CDT(V1:Value, V2:Value, V3:Value), _RHO) _])) =>
           #CDT(V1, V2, V3, V4) ... </k>

  rule <k> (V5:Value ~> ([ Clos(#CDT(V1:Value, V2:Value, V3:Value, V4:Value), _RHO) _])) =>
           #CDT(V1, V2, V3, V4, V5) ... </k>

  rule <k> (V6:Value ~>
            ([ Clos(#CDT(V1:Value, V2:Value, V3:Value, V4:Value, V5:Value), _RHO) _])) =>
           #CDT(V1, V2, V3, V4, V5, V6) ... </k>

  rule <k> #CDT((con data { Constr _Ii:Int [ _DL:DataList ] }), C:Value,
                 _M:Value, _L:Value, _Iv:Value, _B:Value) => C ... </k>

  rule <k> #CDT((con data { Map [ _DL:DataPairList ] }), _C:Value,
                 M:Value, _L:Value, _I:Value, _B:Value) => M ... </k>

  rule <k> #CDT((con data { List [ _DL:DataList ] }), _C:Value,
                 _M:Value, L:Value, _I:Value, _B:Value) => L ... </k>

  rule <k> #CDT((con data { Integer _I:Int }), _C:Value,
                 _M:Value, _L:Value, I:Value, _B:Value) => I ... </k>

  rule <k> #CDT((con data { ByteString _B:ByteString }), _C:Value,
                 _M:Value, _L:Value, _I:Value, B:Value) => B ... </k>

```

## `fstPair`

```k
  rule <k> (builtin fstPair) ~> Force => #FPR ... </k>

  rule <k> (V:Value ~> ([ Clos(#FPR, _RHO) _])) => #FPR(V) ... </k>

  rule <k> #FPR((con pair (T1:TypeConstant) (_T2:TypeConstant)
                          (C1:Constant, _C2:Constant))) => (con T1 C1) ... </k>
```

## `sndPair`

```k
  rule <k> (builtin sndPair) ~> Force => #SPR ... </k>

  rule <k> (V:Value ~> ([ Clos(#SPR, _RHO) _])) => #SPR(V) ... </k>

  rule <k> #SPR((con pair (_T1:TypeConstant) (T2:TypeConstant)
                          (_C1:Constant, C2:Constant))) => (con T2 C2) ... </k>
```

## `chooseList`

```k
  rule <k> (builtin chooseList) ~> Force => #CLT ... </k>

  rule <k> (V:Value ~> ([ Clos(#CLT, _RHO) _])) => #CLT(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CLT(V1:Value), _RHO) _])) => #CLT(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#CLT(V1:Value, V2:Value), _RHO) _])) => #CLT(V1, V2, V3) ... </k>

  rule <k> #CLT((con list(T:TypeConstant) [ .ConstantList ]), (con T C:Constant), _V:Value) =>  (con T C) ... </k>

  rule <k> #CLT((con list(T:TypeConstant) [ _L:ConstantList ] ), _V:Value,  (con T C:Constant)) => (con T C) ... </k> [owise]
```

## `mkCons`

```k
  rule <k> (builtin mkCons) ~> Force => #MCN ... </k>

  rule <k> (V:Value ~> ([ Clos(#MCN, _RHO) _])) => #MCN(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#MCN(V1:Value), _RHO) _])) => #MCN(V1, V2) ... </k>

  rule <k> #MCN((con T:TypeConstant C:Constant), (con list(T:TypeConstant) [ L:ConstantList ])) => (con list(T:TypeConstant) [ C , L ]) </k>
```

## `headList`

```k
  rule <k> (builtin headList) ~> Force => #HLT ... </k>

  rule <k> (V:Value ~> ([ Clos(#HLT, _RHO) _])) => #HLT(V) ... </k>

  rule <k> #HLT((con list(T:TypeConstant) [ C:Constant  , _L:ConstantList ])) => (con T C) </k>
```

## `tailList`

```k
  rule <k> (builtin tailList) ~> Force => #TLT ... </k>

  rule <k> (V:Value ~> ([ Clos(#TLT, _RHO) _])) => #TLT(V) ... </k>

  rule <k> #TLT((con list(T:TypeConstant) [ .ConstantList ])) => (con T [ .ConstantList ]) </k>

  rule <k> #TLT((con list(T:TypeConstant) [ _C:Constant , L:ConstantList ])) => (con T [ L ]) </k>
```

## `nullList`

```k
  rule <k> (builtin nullList) ~> Force => #NLT ... </k>

  rule <k> (V:Value ~> ([ Clos(#NLT, _RHO) _])) => #NLT(V) ... </k>

  rule <k> #NLT((con list(_T:TypeConstant) [ .ConstantList ])) => (con bool True) </k>

  rule <k> #NLT(_V:Value) => (con bool False) </k> [owise]
```

```k
endmodule
``` 
