Cryptographic Primitives
========================

Here we implement the various cryptographic primitives needed for KEVM.

Cryptographic Hashes
--------------------

``` {.k .cryptography-hashes}
module HASH
    imports STRING-SYNTAX
    imports BYTES-SYNTAX
```

For each hash function, we support two overloads -- `String -> String` and
`Bytes -> Bytes`:

The `Bytes -> Bytes` overload returns:

- 32-byte long bytestring for `Sha2_256`
- 32-byte long bytestring for `Sha3_256`
- 32-byte long bytestring for `Keccak256`
- 20-byte long bytestring for `RipEmd160`

``` {.k .cryptography-hashes}
    syntax Bytes  ::= Keccak256 ( Bytes )       [function, hook(HASH.keccak256)]
                    | "Sha2_256" "(" Bytes ")"  [function, hook(HASH.sha2_256 )]
                    | "Sha3_256" "(" Bytes ")"  [function, hook(HASH.sha3_256 )]
                    | RipEmd160 ( Bytes )       [function, hook(HASH.ripemd160)]
```

The `String -> String` overload returns:

- 64-character hex-encoded string for `Sha2_256`
- 64-character hex-encoded string for `Sha3_256`
- 64-character hex-encoded string for `Keccak256`
- 40-character hex-encoded string for `RipEmd160`

``` {.k .cryptography-hashes}
    syntax String ::= Keccak256 ( String )      [function, hook(HASH.keccak256)]
                    | "Sha2_256" "(" String ")" [function, hook(HASH.sha2_256 )]
                    | "Sha3_256" "(" String ")" [function, hook(HASH.sha3_256 )]
                    | RipEmd160 ( String )      [function, hook(HASH.ripemd160)]
```

``` {.k .cryptography-hashes}
endmodule
```

## Elliptic-curve cryptography

```k
module CRYPTOGRAPHY-ELLIPTIC-CURVE
    imports STRING-SYNTAX
    imports INT-SYNTAX
    imports LIST
```

`ECDSARecover` takes a 32-character byte string of a message, v, r, s of the signed message and returns the 64-character public key used to sign the message.
See [this StackOverflow post](https://ethereum.stackexchange.com/questions/15766/what-does-v-r-s-in-eth-gettransactionbyhash-mean) for some information about v, r, and s.

```k
    syntax String ::= ECDSARecover ( String , Int , String , String ) [function, hook(KRYPTO.ecdsaRecover)]
```

The BN128 elliptic curve is defined over 2-dimensional points over the fields of zero- and first-degree polynomials modulo a large prime. (x, y) is a point on G1, whereas (x1 x x2, y1 x y2) is a point on G2, in which x1 and y1 are zero-degree coefficients and x2 and y2 are first-degree coefficients. In each case, (0, 0) is used to represent the point at infinity.

-   `BN128Add` adds two points in G1 together,
-   `BN128Mul` multiplies a point in G1 by a scalar.
-   `BN128AtePairing` accepts a list of points in G1 and a list of points in G2 and returns whether the sum of the product of the discrete logarithm of the G1 points multiplied by the discrete logarithm of the G2 points is equal to zero.
-   `isValidPoint` takes a point in either G1 or G2 and validates that it actually falls on the respective elliptic curve.

```k
    syntax G1Point ::= "(" Int "," Int ")"
    syntax G2Point ::= "(" Int "x" Int "," Int "x" Int ")"
    syntax G1Point ::= BN128Add(G1Point, G1Point) [function, hook(KRYPTO.bn128add)]
                     | BN128Mul(G1Point, Int)     [function, hook(KRYPTO.bn128mul)]
 // -------------------------------------------------------------------------------

    syntax Bool ::= BN128AtePairing(List, List) [function, hook(KRYPTO.bn128ate)]
 // -----------------------------------------------------------------------------

    syntax Bool ::= isValidPoint(G1Point) [function, hook(KRYPTO.bn128valid)]
                  | isValidPoint(G2Point) [function, klabel(isValidG2Point), hook(KRYPTO.bn128g2valid)]
 // ---------------------------------------------------------------------------------------------------
endmodule
```

```k
module KRYPTO
    imports HASH
    imports CRYPTOGRAPHY-ELLIPTIC-CURVE
endmodule
```
