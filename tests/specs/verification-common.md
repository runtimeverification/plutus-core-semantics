# Verification Common

```k
requires "uplc.md"
```

## Simplifications

```k
module SIMPLIFICATIONS
  imports BOOL
  imports K-EQUAL
  imports MAP-SYMBOLIC
```

### Map Reasoning

```k
  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification]

  rule
  { M1:Map [ K1 <- V1 ] [ K2 <- V2 ] #Equals
    M2:Map [ K1 <- V3 ] [ K2 <- V4 ] } =>
  {
    true #Equals
            V1 ==K V3 andBool V2 ==K V4
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ]
  } [simplification]

  rule
  { M1:Map [ K1 <- V1 ] [ K2 <- V2 ] [ K3 <- V3 ] #Equals
    M2:Map [ K1 <- V4 ] [ K2 <- V5 ] [ K3 <- V6 ] } =>
  {
    true #Equals
            V1 ==K V4 andBool V2 ==K V5 andBool V3 ==K V6
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ]
  } [simplification]

  rule removeAll(RHO, keys(RHO)) => .Map [simplification]

endmodule
```

## Main module

```k
module VERIFICATION-COMMON
  imports SIMPLIFICATIONS
  imports UPLC-WITH-LOCAL-GLOBAL-ENV

endmodule
```
