# UPLC Data builtins

```k
require "uplc-configuration.md"

module UPLC-DATA-BUILTINS
  imports UPLC-CONFIGURATION
  imports K-EQUAL

  syntax DataList ::= #mkDataList(ConstantList) [function]
  rule #mkDataList(.ConstantList) => .DataList
  rule #mkDataList(({ T:TextualData }, L:ConstantList)) => (T, #mkDataList(L))

  syntax DataPairList ::= #mkDataPairList(ConstantList) [function]
  rule #mkDataPairList(.ConstantList) => .DataPairList
  rule #mkDataPairList( ( ( {T1:TextualData}, {T2:TextualData} ), L:ConstantList ) ) =>
       ((T1, T2) , #mkDataPairList(L))

  syntax ConstantList ::= #mkConstantList(DataList) [function]
  rule #mkConstantList(.DataList) => .ConstantList
  rule #mkConstantList( (T:TextualData, L:DataList) ) => ({ T }, #mkConstantList(L))

  syntax ConstantList ::= #mkConstantListFromDataPairList(DataPairList) [function]
  rule #mkConstantListFromDataPairList(.DataPairList) => .ConstantList
  rule #mkConstantListFromDataPairList(
         ( (T1:TextualData, T2:TextualData), L:DataPairList ) ) =>
       (({ T1 }, { T2 }), #mkConstantListFromDataPairList(L))
```

## `chooseData`

```k
  rule #expectedArguments(chooseData) => ListItem(forall.a*) ListItem(data) ListItem(a*) ListItem(a*) ListItem(a*) ListItem(a*) ListItem(a*)

  rule <k> #eval(chooseData,
                     (ListItem(< con data { Constr _Ii:Int [ _DL:DataList ] } >)
                      ListItem(C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value))) => C ... </k>

  rule <k> #eval(chooseData,
                     (ListItem(< con data { Map [ _DL:DataPairList ] } >)
                      ListItem(_C:Value)
                      ListItem(M:Value)
                      ListItem(_L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value))) => M ... </k>

  rule <k> #eval(chooseData,
                     (ListItem(< con data { List [ _DL:DataList ] } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(L:Value)
                      ListItem(_Iv:Value)
                      ListItem(_B:Value))) => L ... </k>

  rule <k> #eval(chooseData,
                     (ListItem(< con data { Integer _ } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(I:Value)
                      ListItem(_B:Value))) => I ... </k>

  rule <k> #eval(chooseData,
                     (ListItem(< con data { ByteString _ } >)
                      ListItem(_C:Value)
                      ListItem(_M:Value)
                      ListItem(_L:Value)
                      ListItem(_I:Value)
                      ListItem(B:Value))) => B ... </k>
```

## `constrData`

```k
  rule #expectedArguments(constrData) => ListItem(integer) ListItem(list(data))

  rule <k> #eval(constrData,
                 (ListItem(< con integer I:Int >)
                  ListItem(< con list(data) [ L:ConstantList ] > ))) =>
           (con data { Constr I [ #mkDataList(L) ] }) ... </k>
```

## `mapData`

```k
  rule #expectedArguments(mapData) => ListItem(list(pair(data)(data)))

  rule <k> #eval(mapData,
                 ListItem(< con list(pair(data)(data)) [ L:ConstantList ] >)) =>
           (con data { Map [ #mkDataPairList(L) ] }) ... </k>
```

## `listData`

```k
  rule #expectedArguments(listData) => ListItem(list(data))

  rule <k> #eval(listData,
                 ListItem(< con list(data) [ L:ConstantList ] >)) =>
           (con data { List [ #mkDataList(L) ] }) ... </k>
```

## `iData`

```k
  rule #expectedArguments(iData) => ListItem(integer)

  rule <k> #eval(iData,
                 ListItem(< con integer I:Int >)) =>
           (con data { Integer I }) ... </k>
```

## `bData`

```k
  rule #expectedArguments(bData) => ListItem(bytestring)

  rule <k> #eval(bData,
                 ListItem(< con bytestring B:ByteString >)) =>
           (con data { ByteString B }) ... </k>
```

## `unConstrData`

```k
  rule #expectedArguments(unConstrData) => ListItem(data)

  rule <k> #eval(unConstrData,
                 ListItem(< con data { Constr I:Int [ L:DataList ] } >)) =>
           (con pair(integer)(list(data)) (I, [ #mkConstantList(L) ])) ... </k>
```

## `unMapData`

```k
  rule #expectedArguments(unMapData) => ListItem(data)

  rule <k> #eval(unMapData,
                 ListItem(< con data { Map [ L:DataPairList ] } >)) =>
           (con list(pair (data)(data)) [ #mkConstantListFromDataPairList(L) ] ) ... </k>
```

## `unListData`

```k
  rule #expectedArguments(unListData) => ListItem(data)

  rule <k> #eval(unListData,
                 ListItem(< con data { List [ L:DataList ] } >)) =>
           (con list(data) [ #mkConstantList(L) ]) ... </k>
```

## `unIData`

```k
  rule #expectedArguments(unIData) => ListItem(data)

  rule <k> #eval(unIData,
                 ListItem(< con data { Integer I:Int } >)) =>
           (con integer I) ... </k>
```

## `unBData`

```k
  rule #expectedArguments(unBData) => ListItem(data)

  rule <k> #eval(unBData,
                 ListItem(< con data { ByteString B:ByteString } >)) =>
           (con bytestring B) ... </k>
```

# `equalsData`

```k
  rule #expectedArguments(equalsData) => ListItem(data) ListItem(data)

  rule <k> #eval(equalsData,
                 (ListItem(< con data { T1:TextualData } >)
                  ListItem(< con data { T2:TextualData } >))) =>
           (con bool True) ... </k>
  requires T1 ==K T2

  rule <k> #eval(equalsData,
                 (ListItem(< con data { T1:TextualData } >)
                  ListItem(< con data { T2:TextualData } >))) =>
           (con bool False) ... </k>
  requires T1 =/=K T2
```

# `mkPairData`

```k
  rule #expectedArguments(mkPairData) => ListItem(data) ListItem(data)

  rule <k> #eval(mkPairData,
                 (ListItem(< con data { T1:TextualData } >)
                  ListItem(< con data { T2:TextualData } >))) =>
           (con pair(data)(data) ( { T1 }, { T2 })) ... </k>
```

# `mkNilData`

```k
  rule #expectedArguments(mkNilData) => ListItem(unit)

  rule <k> #eval(mkNilData,
                 ListItem(< con unit () >)) =>
           (con list(data) [ .ConstantList ]) ... </k>
```

# `mkNilPairData`

```k
  rule #expectedArguments(mkNilPairData) => ListItem(unit)
  
  rule <k> #eval(mkNilPairData,
                 ListItem(< con unit () >)) =>
           (con list(pair (data) (data)) [ .ConstantList ]) ... </k>
```

```k
endmodule
```
