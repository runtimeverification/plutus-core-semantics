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

```k
endmodule
``` 
