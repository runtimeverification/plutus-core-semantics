# UPLC Data builtins

```k
require "uplc-configuration.md"

module UPLC-DATA-BUILTINS
  imports UPLC-CONFIGURATION

  syntax DataList ::= mkDataList(ConstantList) [function]
  rule mkDataList(.ConstantList) => .DataList
  rule mkDataList(({ T:TextualData }, L:ConstantList)) => (T, mkDataList(L))
```

## `constrData`

```k
  rule <k> (builtin constrData) => < builtin constrData .List 2 > ... </k>

  rule <k> #eval(constrData,
                 (ListItem(< con integer I:Int >)
                  ListItem(< con list(data) [ L:ConstantList ] > ))) =>
           (con data { Constr I [ mkDataList(L) ] }) ... </k>
```

```k
endmodule
```