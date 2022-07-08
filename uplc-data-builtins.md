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
  rule #numArgs(chooseData) => 6

  rule #typeCheck(ListItem(< con data{ _ } >), chooseData, 1) => true
  
  rule #typeCheck(ListItem(< con data{ _ } >)ListItem( _:Value ), chooseData, 2) => true
  
  rule #typeCheck(ListItem(< con data{ _ } >)ListItem( _:Value )ListItem( _:Value ), chooseData, 3) => true
  
  rule #typeCheck(ListItem(< con data{ _ } >)ListItem( _:Value )ListItem( _:Value )ListItem( _:Value ), chooseData, 4) => true
  
  rule #typeCheck(ListItem(< con data{ _ } >)ListItem( _:Value )ListItem( _:Value )ListItem( _:Value )ListItem( _:Value ), chooseData, 5) => true
  
  rule #typeCheck(ListItem(< con data{ _ } >)ListItem( _:Value )ListItem( _:Value )ListItem( _:Value )ListItem( _:Value )ListItem( _:Value ), chooseData, 6) => true
  
  rule <k> (builtin chooseData) ~> Force => < builtin chooseData .List 6 > ... </k>
       <env> _ => .Map </env>

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
  rule #numArgs(constrData) => 2

  rule #typeCheck(ListItem(< con integer _ >), constrData, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con list(data)[ _ ] >), constrData, 2) => true

  rule <k> (builtin constrData) => < builtin constrData .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(constrData,
                 (ListItem(< con integer I:Int >)
                  ListItem(< con list(data) [ L:ConstantList ] > ))) =>
           (con data { Constr I [ #mkDataList(L) ] }) ... </k>
```

## `mapData`

```k
  rule #numArgs(mapData) => 1

  rule #typeCheck(ListItem(< con list(pair(data)(data))[ _ ] >), mapData, 1) => true

  rule <k> (builtin mapData) => < builtin mapData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(mapData,
                 ListItem(< con list(pair(data)(data)) [ L:ConstantList ] >)) =>
           (con data { Map [ #mkDataPairList(L) ] }) ... </k>
```

## `listData`

```k
  rule #numArgs(listData) => 1

  rule #typeCheck(ListItem(< con list(data)[ _ ] >), listData, 1) => true

  rule <k> (builtin listData) => < builtin listData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(listData,
                 ListItem(< con list(data) [ L:ConstantList ] >)) =>
           (con data { List [ #mkDataList(L) ] }) ... </k>
```

## `iData`

```k
  rule #numArgs(iData) => 1

  rule #typeCheck(ListItem(< con integer _ >), iData, 1) => true

  rule <k> (builtin iData) => < builtin iData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(iData,
                 ListItem(< con integer I:Int >)) =>
           (con data { Integer I }) ... </k>
```

## `bData`

```k
  rule #numArgs(bData) => 1

  rule #typeCheck(ListItem(< con bytestring _ >), bData, 1) => true

  rule <k> (builtin bData) => < builtin bData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(bData,
                 ListItem(< con bytestring B:ByteString >)) =>
           (con data { ByteString B }) ... </k>
```

## `unConstrData`

```k
  rule #numArgs(unConstrData) => 1

  rule #typeCheck(ListItem(< con data{ _ } >), unConstrData, 1) => true

  rule <k> (builtin unConstrData) => < builtin unConstrData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(unConstrData,
                 ListItem(< con data { Constr I:Int [ L:DataList ] } >)) =>
           (con pair(integer)(list(data)) (I, [ #mkConstantList(L) ])) ... </k>
```

## `unMapData`

```k
  rule #numArgs(unMapData) => 1

  rule #typeCheck(ListItem(< con data{ _ } >), unMapData, 1) => true

  rule <k> (builtin unMapData) => < builtin unMapData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(unMapData,
                 ListItem(< con data { Map [ L:DataPairList ] } >)) =>
           (con list(pair (data)(data)) [ #mkConstantListFromDataPairList(L) ] ) ... </k>
```

## `unListData`

```k
  rule #numArgs(unListData) => 1

  rule #typeCheck(ListItem(< con data{ _ } >), unListData, 1) => true

  rule <k> (builtin unListData) => < builtin unListData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(unListData,
                 ListItem(< con data { List [ L:DataList ] } >)) =>
           (con list(data) [ #mkConstantList(L) ]) ... </k>
```

## `unIData`

```k
  rule #numArgs(unIData) => 1

  rule #typeCheck(ListItem(< con data{ _ } >), unIData, 1) => true

  rule <k> (builtin unIData) => < builtin unIData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(unIData,
                 ListItem(< con data { Integer I:Int } >)) =>
           (con integer I) ... </k>
```

## `unBData`

```k
  rule #numArgs(unBData) => 1

  rule #typeCheck(ListItem(< con data{ _ } >), unBData, 1) => true

  rule <k> (builtin unBData) => < builtin unBData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(unBData,
                 ListItem(< con data { ByteString B:ByteString } >)) =>
           (con bytestring B) ... </k>
```

# `equalsData`

```k
  rule #numArgs(equalsData) => 2

  rule #typeCheck(ListItem(< con data{ _ } >), equalsData, 1) => true

  rule #typeCheck(ListItem(< con data{ _ } >)ListItem(< con data{ _ } >), equalsData, 2) => true

  rule <k> (builtin equalsData) => < builtin equalsData .List 2 > ... </k>
       <env> _ => .Map </env>

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
  rule #numArgs(mkPairData) => 2

  rule #typeCheck(ListItem(< con data{ _ } >), mkPairData, 1) => true

  rule #typeCheck(ListItem(< con data{ _ } >)ListItem(< con data{ _ } >), mkPairData, 2) => true

  rule <k> (builtin mkPairData) => < builtin mkPairData .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(mkPairData,
                 (ListItem(< con data { T1:TextualData } >)
                  ListItem(< con data { T2:TextualData } >))) =>
           (con pair(data)(data) ( { T1 }, { T2 })) ... </k>
```

# `mkNilData`

```k
  rule #numArgs(mkNilData) => 1

  rule #typeCheck(ListItem(< con unit _ >), mkNilData, 1) => true

  rule <k> (builtin mkNilData) => < builtin mkNilData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(mkNilData,
                 ListItem(< con unit () >)) =>
           (con list(data) [ .ConstantList ]) ... </k>
```

# `mkNilPairData`

```k
  rule #numArgs(mkNilPairData) => 1
  
  rule #typeCheck(ListItem(< con unit _ >), mkNilPairData, 1) => true
  
  rule <k> (builtin mkNilPairData) => < builtin mkNilPairData .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(mkNilPairData,
                 ListItem(< con unit () >)) =>
           (con list(pair (data) (data)) [ .ConstantList ]) ... </k>
```

```k
endmodule
```