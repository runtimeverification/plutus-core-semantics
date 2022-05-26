# String builtins 

```k
require "uplc-configuration.md"
require "uplc-bytestring.md"
require "uplc-string.md"

module UPLC-STRING-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-BYTESTRING
  imports UPLC-STRING
  imports K-EQUAL
```

## `encodeUtf8`

```k
  rule #numArgs(encodeUtf8) => 1

  rule #typeCheck(ListItem(< con string _ >), encodeUtf8, 1) => true

  rule <k> (builtin encodeUtf8) => < builtin encodeUtf8 .List 1 > ... </k>

  rule <k> #eval(encodeUtf8, ListItem(< con string S:String >)) =>
           < con bytestring #encodeUtf8(S) > ... </k>
```

## `decodeUtf8`

```k 
  rule #numArgs(decodeUtf8) => 1

  rule #typeCheck(ListItem(< con string _ >), decodeUtf8, 1) => true

  rule <k> (builtin decodeUtf8) => < builtin decodeUtf8 .List 1 > ... </k>

  rule <k> #eval(decodeUtf8, ListItem(< con bytestring B:ByteString >)) =>
           < con string #decodeUtf8(B) > ... </k>
```

## `appendString`

```k 
  rule #numArgs(appendString) => 2

  rule #typeCheck(ListItem(< con string _ >), appendString, 1) => true

  rule #typeCheck(ListItem(< con string _ >)ListItem(< con string _ >), appendString, 2) => true

  rule <k> (builtin appendString) => < builtin appendString .List 2 > ... </k>

  rule <k> #eval(appendString,
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >))) =>
           < con string #appendString(S1, S2) > ... </k>
```

## `equalsString`

```k 
  rule #numArgs(equalsString) => 2

  rule #typeCheck(ListItem(< con string _ >), equalsString, 1) => true

  rule #typeCheck(ListItem(< con string _ >)ListItem(< con string _ >), equalsString, 2) => true

  rule <k> (builtin equalsString) => < builtin equalsString .List 2 >  ... </k>

  rule <k> #eval(equalsString,
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >))) =>
           < con bool True > ... </k>
  requires #equalsString(S1, S2)

  rule <k> #eval(equalsString,
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >))) =>
           < con bool False > ... </k>
  requires notBool #equalsString(S1, S2)
```

```k
endmodule
``` 