# UPLC Integer builtins

```k
require "uplc-configuration.md"

module UPLC-INTEGER-BUILTINS
  imports UPLC-CONFIGURATION
  imports K-EQUAL
```

## `addInteger`

```k
  rule <k> (builtin addInteger) => < builtin addInteger .List 2 > ... </k>

  rule <k> < builtin addInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 +Int I2 > ... </k>
```

## `multiplyInteger`

```k
  rule <k> (builtin multiplyInteger) => < builtin multiplyInteger .List 2 > ... </k>

  rule <k> < builtin multiplyInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 *Int I2 > ... </k>
```

## `subtractInteger`

```k
  rule <k> (builtin subtractInteger) => < builtin subtractInteger .List 2 > ... </k>

  rule <k> < builtin subtractInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 -Int I2 > ... </k>
```

## `divideInteger`

According to Plutus specification, `divideInteger` implements standard
mathematical integer division operation.

```k
  rule <k> (builtin divideInteger) => < builtin divideInteger .List 2 > ... </k>

  rule <k> < builtin divideInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> < builtin divideInteger _ 0 > => (error) ... </k> [owise]
```

## `modInteger`

According to Plutus specification, `modInteger` implements standard mathematical integer division operation.

```k
  rule <k> (builtin modInteger) => < builtin modInteger .List 2 > ... </k>

  rule <k> < builtin modInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 modInt I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> < builtin modInteger _ 0 > => (error) ... </k> [owise]
```

## `quotientInteger`

According to Plutus specification, `quotientInteger` rounds towards 0.
According to https://github.com/kframework/k/blob/master/k-distribution/include/kframework/builtin/domains.md
operator `/Int`  computes the quotient using t-division which rounds towards 0.

```k
  rule <k> (builtin quotientInteger) => < builtin quotientInteger .List 2 > ... </k>

  rule <k> < builtin quotientInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> < builtin quotientInteger _ 0 > => (error) ... </k> [owise]
```

## `remainderInteger`

It cooresponds to Haskell rem, according to Plutus specification. From Haskell documentation,
`rem` is integer remainder, satisfying:
(x `quot` y)*y + (x `rem` y) == x


```k
  rule <k> (builtin remainderInteger) => < builtin remainderInteger .List 2 > ... </k>

  rule <k> < builtin remainderInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con integer (I1 -Int (I1 /Int I2) *Int I2) > ... </k>
  requires I2 =/=Int 0

  rule <k> < builtin remainderInteger _ 0 > => (error) ... </k> [owise]
```

## `lessThanInteger`

```k
  rule <k> (builtin lessThanInteger) => < builtin lessThanInteger .List 2 > ... </k>

  rule <k> < builtin lessThanInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con bool True > ... </k>
  requires I1 <Int I2

  rule <k> < builtin lessThanInteger _ 0 > => < con bool False > ... </k> [owise]
```

## `lessThanEqualsInteger`

```k
  rule <k> (builtin lessThanEqualsInteger) =>
           < builtin lessThanEqualsInteger .List 2 > ... </k>

  rule <k> < builtin lessThanEqualsInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con bool True > ... </k>
  requires I1 <=Int I2

  rule <k> < builtin lessThanEqualsInteger _ 0 > => < con bool False > ... </k> [owise]
```

## `equalsInteger`

```k
  rule <k> (builtin equalsInteger) =>
           < builtin equalsInteger .List 2 > ... </k>

  rule <k> < builtin equalsInteger
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >)) 0 > =>
           < con bool True > ... </k>
  requires I1 ==Int I2

  rule <k> < builtin equalsInteger _ 0 > => < con bool False > ... </k> [owise]
```

```k
endmodule
```