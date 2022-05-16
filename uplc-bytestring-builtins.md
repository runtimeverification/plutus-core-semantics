# UPLC Bytestring builtins

```k
require "uplc-configuration.md"
require "uplc-bytestring.md"

module UPLC-BYTESTRING-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-BYTESTRING
  imports K-EQUAL
```

## `appendByteString`


```k
  rule <k> (builtin appendByteString) => < builtin appendByteString .List 2 > ... </k>

  rule <k> #eval(appendByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bytestring #appendByteString(B1, B2) > ... </k>

  rule <k> #eval(appendByteString, _) ~> _ => (error) </k> [owise]
```

## `consByteString`

```k
  rule <k> (builtin consByteString) => < builtin consByteString .List 2 > ... </k>

  rule <k> #eval(consByteString,
                     (ListItem(< con integer I:Int >)
                      ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #consByteString(I, B) > ... </k>

  rule <k> #eval(consByteString, _) ~> _ => (error) </k> [owise]
```

## `sliceByteString`

```k
  rule <k> (builtin sliceByteString) => < builtin sliceByteString .List 3 > ... </k>

  rule <k> #eval(sliceByteString,
                    (ListItem(< con integer I1:Int >)
                     ListItem(< con integer I2:Int >)
                     ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #sliceByteString(I1, I2, B) > ... </k>

  rule <k> #eval(sliceByteString, _) ~> _ => (error) </k> [owise] 
```

## `lengthOfByteString`

```k
  rule <k> (builtin lengthOfByteString) => < builtin lengthOfByteString .List 1 > ... </k>

  rule <k> #eval(lengthOfByteString,
                     ListItem(< con bytestring B:ByteString >)) =>
           < con integer #lengthOfByteString(B) > ... </k>

  rule <k> #eval(lengthOfByteString, _) ~> _ => (error) </k> [owise]
```

## `indexByteString`

```k
  rule <k> (builtin indexByteString) => < builtin indexByteString .List 2 > ... </k>

  rule <k> #eval(indexByteString,
                     (ListItem(< con bytestring B:ByteString >)
                      ListItem(< con integer I:Int >))) =>
           < con integer #indexByteString(B, I) > ... </k>
  requires ((I >=Int 0) andBool (I <=Int (size(mkHexStringList(trimByteString(B))) -Int 1)))

  rule <k> #eval(indexByteString, _) ~> _ => (error) </k> [owise]

```

## `equalsByteString`

```k
  rule <k> (builtin equalsByteString) => < builtin equalsByteString .List 2 > ... </k>

  rule <k> #eval(equalsByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool True > ... </k>
  requires #equalsByteString(B1, B2)

  rule <k> #eval(equalsByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool False > ... </k>
  requires notBool #equalsByteString(B1, B2)

  rule <k> #eval(equalsByteString, _) ~> _ => (error) </k> [owise]
```

## `lessThanByteString`

```k
  rule <k> (builtin lessThanByteString) => < builtin lessThanByteString .List 2 > ... </k>

  rule <k> #eval(lessThanByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool True > ... </k>
  requires #lessThanByteString(B1, B2)

  rule <k> #eval(lessThanByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool False > ... </k>
  requires notBool #lessThanByteString(B1, B2)

  rule <k> #eval(lessThanByteString, _) ~> _ => (error) </k> [owise]
```

## `lessThanEqualsByteString`

```k
  rule <k> (builtin lessThanEqualsByteString) =>
            < builtin lessThanEqualsByteString .List 2 > ... </k>

  rule <k> #eval(lessThanEqualsByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool True > ... </k>
  requires #lessThanEqualsByteString(B1, B2)

  rule <k> #eval(lessThanEqualsByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bool False > ... </k>
  requires notBool #lessThanEqualsByteString(B1, B2)

  rule <k> #eval(lessThanEqualsByteString, _) ~> _ => (error) </k> [owise]
```

```k
endmodule
```
