# UPLC Integer builtins

```k
require "uplc-configuration.md"

module UPLC-INTEGER-BUILTINS
  imports UPLC-CONFIGURATION
  imports K-EQUAL
```  

## `addInteger`

```k 
  rule <k> (builtin addInteger) => #SUM ... </k>

  rule <k> (V:Value ~> ([ Clos(#SUM, _RHO) _])) => #SUM(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#SUM(V2:Value), _RHO) _])) => #SUM(V1, V2) ... </k>

  rule <k> #SUM((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 +Int I2) ... </k>
```

## `multiplyInteger`

```k
  rule <k> (builtin multiplyInteger) => #MUL ... </k>

  rule <k> (V:Value ~> ([ Clos(#MUL, _RHO) _])) => #MUL(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#MUL(V2:Value), _RHO) _])) => #MUL(V1, V2) ... </k>

  rule <k> #MUL((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 *Int I2) ... </k>
```

## `subtractInteger`

```k 
  rule <k> (builtin subtractInteger) => #SUB ... </k>

  rule <k> (V:Value ~> ([ Clos(#SUB, _RHO) _])) => #SUB(V) ... </k>

  rule <k> (V1:Value ~> ([ Clos(#SUB(V2:Value), _RHO) _])) => #SUB(V1, V2) ... </k>

  rule <k> #SUB((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 -Int I2) ... </k>
```

## `divideInteger`

According to Plutus specification, `divideInteger` implements standard mathematical integer division operation.

```k 
  rule <k> (builtin divideInteger) => #DIV ... </k>

  rule <k> (V:Value ~> ([ Clos(#DIV, _RHO) _])) => #DIV(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#DIV(V1:Value), _RHO) _])) => #DIV(V1, V2) ... </k>

  rule <k> #DIV((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 divInt I2) ... </k>
  requires I2 =/=Int 0

  rule <k> #DIV((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0
```

## `modInteger`

According to Plutus specification, `modInteger` implements standard mathematical integer division operation.

```k 
  rule <k> (builtin modInteger) => #MOD ... </k>

  rule <k> (V:Value ~> ([ Clos(#MOD, _RHO) _])) => #MOD(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#MOD(V1:Value), _RHO) _])) => #MOD(V1, V2) ... </k>

  rule <k> #MOD((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 modInt I2) ... </k>
  requires I2 =/=Int 0

  rule <k> #MOD((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0
```

## `quotientInteger`

According to Plutus specification, `quotientInteger` rounds towards 0.
According to https://github.com/kframework/k/blob/master/k-distribution/include/kframework/builtin/domains.md
operator `/Int`  computes the quotient using t-division which rounds towards 0.

```k
  rule <k> (builtin quotientInteger) => #QUO ... </k>

  rule <k> (V:Value ~> ([ Clos(#QUO, _RHO) _])) => #QUO(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#QUO(V1:Value), _RHO) _])) => #QUO(V1, V2) ... </k>

  rule <k> #QUO((con integer I1:Int), (con integer I2:Int)) =>
           (con integer I1 /Int I2) ... </k>
  requires I2 =/=Int 0

  rule <k> #QUO((con integer _I1:Int), (con integer I2:Int)) =>
           (error) ... </k>
  requires I2 ==Int 0
```

## `remainderInteger`

It cooresponds to Haskell rem, according to Plutus specification. From Haskell documentation,
`rem` is integer remainder, satisfying:
(x `quot` y)*y + (x `rem` y) == x


```k 
  rule <k> (builtin remainderInteger) => #REM ... </k>

  rule <k> (V:Value ~> ([ Clos(#REM, _RHO) _])) => #REM(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#REM(V1:Value), _RHO) _])) => #REM(V1, V2) ... </k>

  rule <k> #REM((con integer I1:Int), (con integer I2:Int)) =>
           (con integer (I1 -Int (I1 /Int I2) *Int I2)) ... </k>
  requires I2 =/=Int 0

  rule <k> #REM(_,_) => (error) ... </k> [owise]
```

## `lessThanInteger`

```k
  rule <k> (builtin lessThanInteger) => #LTI ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTI, _RHO) _])) => #LTI(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTI(V1:Value), _RHO) _])) => #LTI(V1, V2) ... </k>

  rule <k> #LTI((con integer I1:Int), (con integer I2:Int)) => (con bool True) ... </k>
  requires I1 <Int I2

  rule <k> #LTI(_,_) => (con bool False) ... </k> [owise]
```

## `lessThanEqualsInteger`

```k
  rule <k> (builtin lessThanEqualsInteger) => #LTE ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTE, _RHO) _])) => #LTE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTE(V1:Value), _RHO) _])) => #LTE(V1, V2) ... </k>

  rule <k> #LTE((con integer I1:Int), (con integer I2:Int)) => (con bool True) ... </k>
  requires I1 <=Int I2

  rule <k> #LTE(_,_) => (con bool False) ... </k> [owise]
```

## `equalsInteger`

```k
  rule <k> (builtin equalsInteger) => #EQI ... </k>
  rule <k> (V:Value ~> ([ Clos(#EQI, _RHO) _])) => #EQI(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#EQI(V1:Value), _RHO) _])) => #EQI(V1, V2) ... </k>

  rule <k> #EQI((con integer I1:Int), (con integer I2:Int)) => (con bool True) ... </k>
  requires I1 >=Int I2 

  rule <k> #EQI(_,_) => (con bool False) ... </k> [owise]
```

```k 
endmodule
```