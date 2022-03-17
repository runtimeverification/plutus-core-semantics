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

  rule <k> < builtin appendByteString
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >)) 0 > =>
           < con bytestring #appendByteString(B1, B2) > ... </k>
```

## `consByteString`

```k
  rule <k> (builtin consByteString) => < builtin consByteString .List 2 > ... </k>

  rule <k> < builtin consByteString
                     (ListItem(< con integer I:Int >)
                      ListItem(< con bytestring B:ByteString >)) 0 > =>
           < con bytestring #consByteString(I, B) > ... </k>
```

## `sliceByteString`

```k
  rule <k> (builtin sliceByteString) => < builtin sliceByteString .List 3 > ... </k>

  rule <k> <builtin sliceByteString
                    (ListItem(< con integer I1:Int >)
                     ListItem(< con integer I2:Int >)
                     ListItem(< con bytestring B:ByteString >)) 0 > =>
           < con bytestring #sliceByteString(I1, I2, B) > ... </k>
```

## `lengthOfByteString`

```k
  rule <k> (builtin lengthOfByteString) => < builtin lengthOfByteString .List 1 > ... </k>

  rule <k> < builtin lengthOfByteString
                     ListItem(< con bytestring B:ByteString >) 0 > =>
           < con integer #lengthOfByteString(B) > ... </k>
```

## `indexByteString`

```k
  rule <k> (builtin indexByteString) => < builtin indexByteString .List 2 > ... </k>

  rule <k> < builtin indexByteString
                     (ListItem(< con bytestring B:ByteString >)
                      ListItem(< con integer I:Int >)) 0 > =>
           < con integer #indexByteString(B, I) > ... </k>
```

## `equalsByteString`

```k
  rule <k> (builtin equalsByteString) => < builtin equalsByteString .List 2 > ... </k>

  rule <k> < builtin equalsByteString
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >)) 0 > =>
           < con bool True > ... </k>
  requires #equalsByteString(B1, B2)

  rule <k> < builtin equalsByteString _ 0 > =>
           < con bool False > ... </k> [owise]

```

## `lessThanByteString`

```k
  rule <k> (builtin lessThanByteString) => < builtin lessThanByteString .List 2 > ... </k>

  rule <k> < builtin lessThanByteString
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >)) 0 > =>
           < con bool True > ... </k>
  requires #lessThanByteString(B1, B2)

  rule <k> < builtin lessThanByteString _ 0 > => < con bool False > ... </k> [owise]
```

## `lessThanEqualsByteString`

```k
  rule <k> (builtin lessThanEqualsByteString) =>
            < builtin lessThanEqualsByteString .List 2 > ... </k>

  rule <k> < builtin lessThanEqualsByteString
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >)) 0 > =>
           < con bool True > ... </k>
  requires #lessThanEqualsByteString(B1, B2)

  rule <k> < builtin lessThanEqualsByteString _ 0 > =>
           < con bool False > ... </k> [owise]
```

```k
endmodule
```