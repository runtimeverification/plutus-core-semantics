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
  rule <k> (builtin encodeUtf8) => < builtin encodeUtf8 .List 1 > ... </k>

  rule <k> < builtin encodeUtf8 ListItem(< con string S:String >) 0 > =>
           < con bytestring #encodeUtf8(S) > ... </k>
```

## `decodeUtf8`

```k 
  rule <k> (builtin decodeUtf8) => < builtin decodeUtf8 .List 1 > ... </k>

  rule <k> < builtin decodeUtf8 ListItem(< con bytestring B:ByteString >) 0 > =>
           < con string #decodeUtf8(B) > ... </k>
```

## `appendString`

```k 
  rule <k> (builtin appendString) => < builtin appendString .List 2 > ... </k>

  rule <k> < builtin appendString
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >)) 0 > =>
           < con string #appendString(S1, S2) > ... </k>
```

## `equalsString`

```k 
  rule <k> (builtin equalsString) => < builtin equalsString .List 2 >  ... </k>

  rule <k> < builtin equalsString
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >)) 0 > =>
           < con bool True > ... </k>
  requires #equalsString(S1, S2)

  rule <k> < builtin equalsString _ 0 > => < con bool False > ... </k> [owise]
```

```k
endmodule
``` 