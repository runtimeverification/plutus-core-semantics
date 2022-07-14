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
  rule #typeSignature(encodeUtf8) => string

  rule <k> #eval(encodeUtf8, ListItem(< con string S:String >)) =>
           < con bytestring #encodeUtf8(S) > ... </k>
```

## `decodeUtf8`

```k 
  rule #typeSignature(decodeUtf8) => string

  rule <k> #eval(decodeUtf8, ListItem(< con bytestring B:ByteString >)) =>
           < con string #decodeUtf8(B) > ... </k>
```

## `appendString`

```k 
  rule #typeSignature(appendString) => string string

  rule <k> #eval(appendString,
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >))) =>
           < con string #appendString(S1, S2) > ... </k>
```

## `equalsString`

```k 
  rule #typeSignature(equalsString) => string string

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
