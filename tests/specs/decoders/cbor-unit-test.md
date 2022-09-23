```k
requires "verification.md"

module CBOR-UNIT-TEST
  imports VERIFICATION

  claim <k> simplify( DHead( 0, BitStream( 8, String2Bytes( "\x00" ) ) ) ) =>
    simplified( DH( BitStream( 8 , b"\x00" ) , 0 , 0 ) ) </k>

  claim <k> simplify( DHead( 25 , BitStream( 8, String2Bytes( "\x19\x01\x00" ) ) ) ) =>
    simplified( DH( BitStream( 24 , b"\x19\x01\x00" ) , 0 , 256 ) ) </k>

  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\x00" ) ) ) ) =>
    simplified( BTPair( BitStream ( 8 , b"\x00" ) , Integer 0 ) ) </k>

  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\x19\x01\x00" ) ) ) ) =>
    simplified( BTPair( BitStream ( 24 , b"\x19\x01\x00" ) , Integer 256 ) ) </k>

  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\x3a\x3a\xde\x68\xb0" ) ) ) ) =>
    simplified( BTPair( BitStream ( 40 , b"::\xdeh\xb0" ) , Integer -987654321 ) ) </k>
```

ByteStrings of length 0 - 64

```k
  claim <k> simplify( DBlock( BitStream( 0, String2Bytes( "\x40" ) ) ) ) =>
    simplified( BBPair( BitStream ( 8 , b"\x40" ) , String2Bytes("") ) ) </k>

  claim <k> simplify( DBlock( BitStream( 0, String2Bytes( "\x58\x40" ) +Bytes Int2Bytes( 64, 1234567890, BE ) ) ) ) =>
    simplified( BBPair(
                  BitStream( 528 , String2Bytes( "\x58\x40" ) +Bytes Int2Bytes( 64, 1234567890, BE )),
                  Int2Bytes( 64, 1234567890, BE ) ) ) </k>
```

ByteStrings longer than 64 bytes

```k
  claim <k> simplify( DBlocks( BitStream( 0, #65_BYTES_OF_0 ) ) ) =>
            simplified( BBPair( BitStream ( 552 , #65_BYTES_OF_0 ), Int2Bytes( 65, 0, BE ) ) ) </k>

  claim <k> simplify( DBStar( BitStream( 0, #65_BYTES_OF_0_WITH_HEAD ) ) ) =>
            simplified( BBPair( BitStream ( 560, #65_BYTES_OF_0_WITH_HEAD ), Int2Bytes( 65, 0, BE ) ) ) </k>
```

Integers with size larger than 64 bits.

```k
  claim <k> simplify( DData( BitStream( 0, #LARGE_POSITIVE_INT ) ) ) =>
            simplified( BTPair( BitStream( lengthBytes( #LARGE_POSITIVE_INT ) *Int 8 , #LARGE_POSITIVE_INT ),
                                Integer 18446744073709551616 ) ) </k>

  claim <k> simplify( DData( BitStream( 0, #LARGE_NEGATIVE_INT ) ) ) =>
            simplified( BTPair( BitStream( lengthBytes( #LARGE_NEGATIVE_INT ) *Int 8 , #LARGE_NEGATIVE_INT ),
                                Integer -18446744073709551617 ) ) </k>
```

ByteString constructors

```k
  claim <k> simplify( DData( BitStream( 0, #BYTESTRING_BYTES ) ) ) =>
            simplified( BTPair( BitStream( lengthBytes( #BYTESTRING_BYTES ) *Int 8 , #BYTESTRING_BYTES ),
                                ByteString String2ByteString( "#000102030405060708090a0b0c0d0e0f" ) ) ) </k>
```

Empty `List []` constructor

```k
  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\x80" ) ) ) ) =>
            simplified( BTPair( BitStream( 8 , String2Bytes( "\x80" ) ),
                                List [ .DataList ] ) ) </k>
```

Empty `List []` constructor

```k
  claim <k> simplify( DDataStar( BitStream( 0, String2Bytes( "\x80" ) ) ) ) =>
            simplified( BTPair( BitStream( 8 , String2Bytes( "\x80" ) ),
                                List [ .DataList ] ) ) </k>
```


Constr 0 []

```k
  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\xD8\x79\x80" ) ) ) ) =>
            simplified( BTPair( BitStream( 24, String2Bytes( "\xD8\x79\x80" )) , Constr 0 [ .DataList ] )) </k>


  claim <k> simplify( DecodeCborData( String2ByteString( "#D87980" ) ) ) =>
            simplified( Constr 0 [ .DataList ] ) </k>
```

Map constructor
```k
  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\xA0" ) ) ) ) =>
            simplified( BTPair( BitStream( 8, String2Bytes( "\xA0" )),  Map [ .DataPairList ] ) ) </k>


  claim <k> simplify( DData( BitStream( 0, String2Bytes( "\xA1\x40\xA1\x40\x1A\x05\xF5\xE1\x00" ) ) ) ) =>
            simplified( BTPair( BitStream( 72, String2Bytes( "\xA1\x40\xA1\x40\x1A\x05\xF5\xE1\x00" ) ),
              Map [
                ( ByteString String2ByteString("#"),
                  Map [ (ByteString String2ByteString("#"), Integer 100000000 ) ] ) ] ) ) </k>

```

Example cbor encoding from trivial minting policy

```k
  claim <k> simplify( DecodeCborData(
String2ByteString("#d8799fd8799f9fd8799fd8799fd8799f582098d5fbcefe21113b3f0390c1441e075b8a870cc5a8fa2a56dcde1d8247e41715ff02ffd8799fd8799fd8799f581c80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7ffd87a80ffa140a1401a05f5e100d87a80ffffff9fd8799fd8799fd8799f581c80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7ffd87a80ffa240a1401a05d7529b581c983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63a0d87a80ffd8799fd8799fd8799f581c80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7ffd87a80ffa2581c983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63a1434142431901bc40a1401a001e8480d87a80ffffa140a1401909e5a1581c983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63a1434142431901bc8080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581c80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7ff80d8799f582088af065d723e87beb6cdce342594729cbb62c5bb8a440df29d6359a479f8bfacffffd8799f581c983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63ffff")
)) =>
simplified(
  Constr 0
  [
    Constr 0
    [
      List
      [
        Constr 0
        [
          Constr 0
          [
            Constr 0 [ ByteString String2ByteString("#98d5fbcefe21113b3f0390c1441e075b8a870cc5a8fa2a56dcde1d8247e41715")], Integer 2
          ],
          Constr 0
          [
            Constr 0
            [
              Constr 0 [ ByteString String2ByteString("#80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7") ],
              Constr 1 [ .DataList ]
            ],
            Map[ ( ByteString String2ByteString("#") , Map[ (ByteString String2ByteString("#") , Integer 100000000 ) ] ) ],
            Constr 1 [ .DataList ]
          ]
        ]
      ],
      List
      [
        Constr 0
        [
          Constr 0
          [
            Constr 0 [ ByteString String2ByteString("#80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7") ],
            Constr 1 [ .DataList ]
          ],
          Map[ (ByteString String2ByteString("#") , Map[ ( ByteString String2ByteString("#") , Integer 97997467 ) ]), (ByteString String2ByteString("#983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63") , Map[ .DataPairList ])],
          Constr 1 [ .DataList ]
        ],
        Constr 0
        [
          Constr 0
          [
            Constr 0 [ ByteString String2ByteString("#80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7") ],
            Constr 1 [ .DataList ]
          ],
          Map
          [
            ( ByteString String2ByteString("#983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63") , Map[ ( ByteString String2ByteString("#414243") , Integer 444 ) ] ),
            ( ByteString String2ByteString("#") , Map[( ByteString String2ByteString("#") , Integer 2000000 )])
          ],
          Constr 1 [ .DataList ]
        ]
      ],
      Map [ ( ByteString String2ByteString("#") , Map[ (ByteString String2ByteString("#") , Integer 2533)]) ],
      Map [ ( ByteString String2ByteString("#983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63") , Map[ ( ByteString String2ByteString("#414243") , Integer 444 ) ] ) ],
      List [ .DataList ],
      List [ .DataList ],
      Constr 0
      [
        Constr 0
        [
          Constr 0 [ .DataList ], Constr 1 [ .DataList ]
        ],
        Constr 0
        [
          Constr 2 [ .DataList ], Constr 1 [ .DataList ]
        ]
      ],
      List [ ByteString String2ByteString("#80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7") ],
      List [ .DataList ],
      Constr 0 [ ByteString String2ByteString("#88af065d723e87beb6cdce342594729cbb62c5bb8a440df29d6359a479f8bfac") ]
    ],
    Constr 0 [ ByteString String2ByteString("#983cf28fd53484a220dc42c0ae430b0c1c16b210a72975c5a98dec63") ]
  ]

) </k>


endmodule
```
