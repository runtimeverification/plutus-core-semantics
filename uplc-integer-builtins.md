# UPLC Integer builtins

```k
require "uplc-configuration.md"

module UPLC-INTEGER-BUILTINS
  imports UPLC-CONFIGURATION
  imports K-EQUAL
```

## `addInteger`

```k
  rule #numArgs(addInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), addInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), addInteger, 2) => true

  rule <k> (builtin addInteger) => < builtin addInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(addInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 +Int I2 > ... </k>
```

## `multiplyInteger`

```k
  rule #numArgs(multiplyInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), multiplyInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), multiplyInteger, 2) => true

  rule <k> (builtin multiplyInteger) => < builtin multiplyInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(multiplyInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 *Int I2 > ... </k>
```

## `subtractInteger`

```k
  rule #numArgs(subtractInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), subtractInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), subtractInteger, 2) => true

  rule <k> (builtin subtractInteger) => < builtin subtractInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(subtractInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 -Int I2 > ... </k>
```

## `divideInteger`

According to Plutus specification, `divideInteger` implements standard
mathematical integer division operation.

```k
  rule #numArgs(divideInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), divideInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), divideInteger, 2) => true

  rule <k> (builtin divideInteger) => < builtin divideInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(divideInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0
```

## `modInteger`

According to Plutus specification, `modInteger` implements standard mathematical integer division operation.

```k
  rule #numArgs(modInteger) => 2
  
  rule #typeCheck(ListItem(< con integer _ >), modInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), modInteger, 2) => true

  rule <k> (builtin modInteger) => < builtin modInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(modInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 modInt I2 > ... </k>
  requires I2 =/=Int 0
```

## `quotientInteger`

According to Plutus specification, `quotientInteger` rounds towards 0.
According to https://github.com/kframework/k/blob/master/k-distribution/include/kframework/builtin/domains.md
operator `/Int`  computes the quotient using t-division which rounds towards 0.

```k
  rule #numArgs(quotientInteger) => 2
  
  rule #typeCheck(ListItem(< con integer _ >), quotientInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), quotientInteger, 2) => true

  rule <k> (builtin quotientInteger) => < builtin quotientInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(quotientInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0
```

## `remainderInteger`

It cooresponds to Haskell rem, according to Plutus specification. From Haskell documentation,
`rem` is integer remainder, satisfying:
(x `quot` y)*y + (x `rem` y) == x


```k
  rule #numArgs(remainderInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), remainderInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), remainderInteger, 2) => true

  rule <k> (builtin remainderInteger) => < builtin remainderInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(remainderInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer (I1 -Int (I1 /Int I2) *Int I2) > ... </k>
  requires I2 =/=Int 0
```

## `lessThanInteger`

```k
  rule #numArgs(lessThanInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), lessThanInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), lessThanInteger, 2) => true

  rule <k> (builtin lessThanInteger) => < builtin lessThanInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(lessThanInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool True > ... </k>
  requires I1 <Int I2

  rule <k> #eval(lessThanInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool False > ... </k>
  requires I1 >=Int I2
```

## `lessThanEqualsInteger`

```k
  rule #numArgs(lessThanEqualsInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), lessThanEqualsInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), lessThanEqualsInteger, 2) => true

  rule <k> (builtin lessThanEqualsInteger) => < builtin lessThanEqualsInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(lessThanEqualsInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool True > ... </k>
  requires I1 <=Int I2

  rule <k> #eval(lessThanEqualsInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool False > ... </k>
  requires I1 >Int I2
```

## `equalsInteger`

```k
  rule #numArgs(equalsInteger) => 2

  rule #typeCheck(ListItem(< con integer _ >), equalsInteger, 1) => true

  rule #typeCheck(ListItem(< con integer _ >)ListItem(< con integer _ >), equalsInteger, 2) => true

  rule <k> (builtin equalsInteger) => < builtin equalsInteger .List 2 > ... </k>
       <env> _ => .Map </env>

  rule <k> #eval(equalsInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool True > ... </k>
  requires I1 ==Int I2

  rule <k> #eval(equalsInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con bool False > ... </k>
  requires I1 =/=Int I2
```

```k
endmodule
```