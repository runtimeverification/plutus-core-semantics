# UPLC Data builtins

```k
require "uplc-configuration.md"

module UPLC-DATA-BUILTINS
  imports UPLC-CONFIGURATION

  syntax DataList ::= mkDataList(ConstantList) [function]
  rule mkDataList(.ConstantList) => .DataList
  rule mkDataList(({ T:TextualData }, L:ConstantList)) => (T, mkDataList(L))

  syntax DataPairList ::= mkDataPairList(ConstantList) [function]
  rule mkDataPairList(.ConstantList) => .DataPairList
  rule mkDataPairList( ( ( {T1:TextualData}, {T2:TextualData} ) , L:ConstantList ) ) =>
       ((T1, T2) , mkDataPairList(L))
```

## `constrData`

```k
  rule <k> (builtin constrData) => < builtin constrData .List 2 > ... </k>

  rule <k> #eval(constrData,
                 (ListItem(< con integer I:Int >)
                  ListItem(< con list(data) [ L:ConstantList ] > ))) =>
           (con data { Constr I [ mkDataList(L) ] }) ... </k>
```

## `mapData`

```k
  rule <k> (builtin mapData) => < builtin mapData .List 1 > ... </k>

  rule <k> #eval(mapData,
                 ListItem(< con list(pair(data)(data)) [ L:ConstantList ] >)) =>
           (con data { Map [ mkDataPairList(L) ] }) ... </k>
```

## `listData`

```k
  rule <k> (builtin listData) => < builtin listData .List 1 > ... </k>

  rule <k> #eval(listData,
                 ListItem(< con list(data) [ L:ConstantList ] >)) =>
           (con data { List [ mkDataList(L) ] }) ... </k>
```

```k
endmodule
```