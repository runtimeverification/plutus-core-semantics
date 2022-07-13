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
  rule #numArgs(appendByteString) => 2
  rule #typeSignature(appendByteString) => bytestring bytestring

  rule <k> #eval(appendByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bytestring #appendByteString(B1, B2) > ... </k>
```

## `consByteString`

```k
  rule #numArgs(consByteString) => 2
  rule #typeSignature(consByteString) => integer bytestring
 
  rule <k> #eval(consByteString,
                     (ListItem(< con integer I:Int >)
                      ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #consByteString(I, B) > ... </k>
```

## `sliceByteString`

```k
  rule #numArgs(sliceByteString) => 3
  rule #typeSignature(sliceByteString) => integer integer bytestring
 
  rule <k> #eval(sliceByteString,
                    (ListItem(< con integer I1:Int >)
                     ListItem(< con integer I2:Int >)
                     ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #sliceByteString(I1, I2, B) > ... </k>
```

## `lengthOfByteString`

```k
  rule #numArgs(lengthOfByteString) => 1
  rule #typeSignature(lengthOfByteString) => bytestring
 
  rule <k> #eval(lengthOfByteString,
                     ListItem(< con bytestring B:ByteString >)) =>
           < con integer #lengthOfByteString(B) > ... </k>
```

## `indexByteString`

```k
  rule #numArgs(indexByteString) => 2
  rule #typeSignature(indexByteString) => bytestring integer

  rule <k> #eval(indexByteString,
                     (ListItem(< con bytestring B:ByteString >)
                      ListItem(< con integer I:Int >))) =>
           < con integer #indexByteString(B, I) > ... </k>
  requires ((I >=Int 0) andBool (I <=Int (size(mkHexStringList(trimByteString(B))) -Int 1)))
```

## `equalsByteString`

```k
  rule #numArgs(equalsByteString) => 2
  rule #typeSignature(equalsByteString) => bytestring bytestring

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
```

## `lessThanByteString`

```k
  rule #numArgs(lessThanByteString) => 2
  rule #typeSignature(lessThanByteString) => bytestring bytestring

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
```

## `lessThanEqualsByteString`

```k
  rule #numArgs(lessThanEqualsByteString) => 2
  rule #typeSignature(lessThanEqualsByteString) => bytestring bytestring

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
```

```k
endmodule
```
