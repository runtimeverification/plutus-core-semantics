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

  rule <k> #eval(addInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 +Int I2 > ... </k>

  rule <k> #eval(addInteger, _) ~> _ => (error) </k> [owise]
```

## `multiplyInteger`

```k
  rule <k> (builtin multiplyInteger) => < builtin multiplyInteger .List 2 > ... </k>

  rule <k> #eval(multiplyInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 *Int I2 > ... </k>

  rule <k> #eval(multiplyInteger, _) ~> _ => (error) </k> [owise]
```

## `subtractInteger`

```k
  rule <k> (builtin subtractInteger) => < builtin subtractInteger .List 2 > ... </k>

  rule <k> #eval(subtractInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 -Int I2 > ... </k>

  rule <k> #eval(subtractInteger, _) ~> _ => (error) </k> [owise]
```

## `divideInteger`

According to Plutus specification, `divideInteger` implements standard
mathematical integer division operation.

```k
  rule <k> (builtin divideInteger) => < builtin divideInteger .List 2 > ... </k>

  rule <k> #eval(divideInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> #eval(divideInteger, _) ~> _ => (error) </k> [owise]
```

## `modInteger`

According to Plutus specification, `modInteger` implements standard mathematical integer division operation.

```k
  rule <k> (builtin modInteger) => < builtin modInteger .List 2 > ... </k>

  rule <k> #eval(modInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 modInt I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> #eval(modInteger, _) ~> _ => (error) </k> [owise]
```

## `quotientInteger`

According to Plutus specification, `quotientInteger` rounds towards 0.
According to https://github.com/kframework/k/blob/master/k-distribution/include/kframework/builtin/domains.md
operator `/Int`  computes the quotient using t-division which rounds towards 0.

```k
  rule <k> (builtin quotientInteger) => < builtin quotientInteger .List 2 > ... </k>

  rule <k> #eval(quotientInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer I1 /Int I2 > ... </k>
  requires I2 =/=Int 0

  rule <k> #eval(quotientInteger, _) ~> _ => (error) </k> [owise]
```

## `remainderInteger`

It cooresponds to Haskell rem, according to Plutus specification. From Haskell documentation,
`rem` is integer remainder, satisfying:
(x `quot` y)*y + (x `rem` y) == x


```k
  rule <k> (builtin remainderInteger) => < builtin remainderInteger .List 2 > ... </k>

  rule <k> #eval(remainderInteger,
                     (ListItem(< con integer I1:Int >)
                      ListItem(< con integer I2:Int >))) =>
           < con integer (I1 -Int (I1 /Int I2) *Int I2) > ... </k>
  requires I2 =/=Int 0

  rule <k> #eval(remainderInteger, _) ~> _ => (error) </k> [owise]
```

## `lessThanInteger`

```k
  rule <k> (builtin lessThanInteger) => < builtin lessThanInteger .List 2 > ... </k>

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

  rule <k> #eval(lessThanInteger, _) ~> _ => (error) </k> [owise]
```

## `lessThanEqualsInteger`

```k
  rule <k> (builtin lessThanEqualsInteger) =>
           < builtin lessThanEqualsInteger .List 2 > ... </k>

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

  rule <k> #eval(lessThanEqualsInteger, _) ~> _ => (error) </k> [owise]
```

## `equalsInteger`

```k
  rule <k> (builtin equalsInteger) =>
           < builtin equalsInteger .List 2 > ... </k>

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

  rule <k> #eval(equalsInteger, _) ~> _ => (error) </k> [owise]
```

```k
endmodule
```