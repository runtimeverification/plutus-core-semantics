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

  rule #typeCheck(ListItem(< con bytestring _ >), appendByteString, 1) => true

  rule #typeCheck(ListItem(< con bytestring _ >) ListItem(< con bytestring _ >),
                  appendByteString, 2) => true

  rule <k> (builtin appendByteString) => < builtin appendByteString .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(appendByteString,
                     (ListItem(< con bytestring B1:ByteString >)
                      ListItem(< con bytestring B2:ByteString >))) =>
           < con bytestring #appendByteString(B1, B2) > ... </k>
```

## `consByteString`

```k
  rule #numArgs(consByteString) => 2
 
  rule #typeCheck(ListItem(< con integer _ >), consByteString, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)
                  ListItem(< con bytestring _ >), consByteString, 2) => true

  rule <k> (builtin consByteString) => < builtin consByteString .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(consByteString,
                     (ListItem(< con integer I:Int >)
                      ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #consByteString(I, B) > ... </k>
```

## `sliceByteString`

```k
  rule #numArgs(sliceByteString) => 3
 
  rule #typeCheck(ListItem(< con integer _ >), sliceByteString, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)
                  ListItem(< con integer _ >), sliceByteString, 2) => true

  rule #typeCheck(ListItem(< con integer _ >)
                  ListItem(< con integer _ >)
                  ListItem(< con bytestring _ >), sliceByteString, 3) => true

  rule <k> (builtin sliceByteString) => < builtin sliceByteString .List 3 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(sliceByteString,
                    (ListItem(< con integer I1:Int >)
                     ListItem(< con integer I2:Int >)
                     ListItem(< con bytestring B:ByteString >))) =>
           < con bytestring #sliceByteString(I1, I2, B) > ... </k>
```

## `lengthOfByteString`

```k
  rule #numArgs(lengthOfByteString) => 1
 
  rule #typeCheck(ListItem(< con bytestring _ >), lengthOfByteString, 1) => true

  rule <k> (builtin lengthOfByteString) => < builtin lengthOfByteString .List 1 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(lengthOfByteString,
                     ListItem(< con bytestring B:ByteString >)) =>
           < con integer #lengthOfByteString(B) > ... </k>
```

## `indexByteString`

```k
  rule #numArgs(indexByteString) => 2

  rule #typeCheck(ListItem(< con bytestring _ >), indexByteString, 1) => true

  rule #typeCheck(ListItem(< con bytestring _ >)
                  ListItem(< con integer _ >), indexByteString, 2) => true
                  
  rule <k> (builtin indexByteString) => < builtin indexByteString .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(indexByteString,
                     (ListItem(< con bytestring B:ByteString >)
                      ListItem(< con integer I:Int >))) =>
           < con integer #indexByteString(B, I) > ... </k>
  requires ((I >=Int 0) andBool (I <=Int (size(mkHexStringList(trimByteString(B))) -Int 1)))
```

## `equalsByteString`

```k
  rule #numArgs(equalsByteString) => 2

  rule #typeCheck(ListItem(< con bytestring _ >), equalsByteString, 1) => true

  rule #typeCheck(ListItem(< con bytestring _ >)ListItem(< con bytestring _ >), equalsByteString, 2) => true

  rule <k> (builtin equalsByteString) => < builtin equalsByteString .List 2 > ... </k>
       <env> _ => .Map </env>

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

  rule #typeCheck(ListItem(< con bytestring _ >), lessThanByteString, 1) => true

  rule #typeCheck(ListItem(< con bytestring _ >)ListItem(< con bytestring _ >), lessThanByteString, 2) => true

  rule <k> (builtin lessThanByteString) => < builtin lessThanByteString .List 2 > ... </k>
       <env> _ => .Map </env>

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

  rule #typeCheck(ListItem(< con bytestring _ >), lessThanEqualsByteString, 1) => true

  rule #typeCheck(ListItem(< con bytestring _ >)ListItem(< con bytestring _ >), lessThanEqualsByteString, 2) => true

  rule <k> (builtin lessThanEqualsByteString) => < builtin lessThanEqualsByteString .List 2 > ... </k>
       <env> _ => .Map </env>

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
