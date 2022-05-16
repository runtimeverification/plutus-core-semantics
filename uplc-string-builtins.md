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

  rule <k> #eval(encodeUtf8, ListItem(< con string S:String >)) =>
           < con bytestring #encodeUtf8(S) > ... </k>

  rule <k> #eval(encodeUtf8, _) ~> _ => (error) </k> [owise]
```

## `decodeUtf8`

```k 
  rule <k> (builtin decodeUtf8) => < builtin decodeUtf8 .List 1 > ... </k>

  rule <k> #eval(decodeUtf8, ListItem(< con bytestring B:ByteString >)) =>
           < con string #decodeUtf8(B) > ... </k>

  rule <k> #eval(decodeUtf8, _) ~> _ => (error) </k> [owise]

```

## `appendString`

```k 
  rule <k> (builtin appendString) => < builtin appendString .List 2 > ... </k>

  rule <k> #eval(appendString,
              (ListItem(< con string S1:String >)
               ListItem(< con string S2:String >))) =>
           < con string #appendString(S1, S2) > ... </k>

  rule <k> #eval(appendString, _) ~> _ => (error) </k> [owise]
```

## `equalsString`

```k 
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

  rule <k> #eval(equalsString, _) ~> _ => (error) </k> [owise]
```

```k
endmodule
``` 