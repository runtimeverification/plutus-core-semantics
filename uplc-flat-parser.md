# Flat Parser

```k
requires "uplc-syntax.md"
requires "bitstream.md"

module UPLC-FLAT-PARSER
  imports UPLC-SYNTAX
  imports BYTES
  imports BITSTREAM
```

## Flat Parser Entrypoint

The following function is the entry point to the flat parser.

```k
  syntax ConcreteProgram ::= #bytes2program( Bytes )     [function]
                           | #bytes2program( String, K ) [function]

  rule #bytes2program( BYTES ) =>
    #let
      VERSION = #readVersion( BitStream( 0, BYTES ) )
    #in
      #bytes2program( #getDatum( VERSION ),
                      #readProgramTerm( #readTerm, BitStream( #getBitLength( VERSION ), BYTES ) )
                    )

  rule #bytes2program( VERSION, TERM:Term ~> . ) => ( program String2Version( VERSION ) TERM )
```

The following KItems are used to manage control in the function above.

```k
  syntax KItem ::= "#readTerm"
                 | "#readTermTag" Int
                 | "#readValue" Int
                 | "#readConType" Int
```

## Parsing The program version

The program version is specified as 3 unsigned integers. This implementation only accounts for
version numbers that are less than 7 bits and needs to be updated to parse larger integers.

```k
  syntax StringDatum ::= SDat( BitLength:Int, Datum:String )

  syntax Int ::= #getBitLength( StringDatum ) [function]
//------------------------------------------------------
  rule #getBitLength( SDat( I, _ ) ) => I

  syntax String ::= #getDatum( StringDatum ) [function]
//-----------------------------------------------------
  rule #getDatum( SDat( _, S ) ) => S

  syntax KItem ::= "#read1stVersion" String
                 | "#read2ndVersion" String
                 | "#read3rdVersion" String

  syntax StringDatum ::= #readVersion( BitStream )    [function]
  syntax StringDatum ::= #readVersion( K, BitStream ) [function]
//--------------------------------------------------------------
  rule #readVersion( BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      #readVersion( #read1stVersion #getDatumAsString( VARDATA ) +String ".",
                    BitStream( I +Int #getBitLength( VARDATA ), BYTES )
                  )

  rule #readVersion( #read1stVersion S, BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      #readVersion( #read2ndVersion S +String #getDatumAsString( VARDATA ) +String ".",
                    BitStream( I +Int #getBitLength( VARDATA ), BYTES )
                  )

  rule #readVersion( #read2ndVersion S, BitStream( I, BYTES ) ) =>
    #let
      VARDATA = #getVarLenData( BitStream( I, BYTES ) )
    #in
      SDat( I +Int #getBitLength( VARDATA ) , S +String #getDatumAsString( VARDATA ) )

  syntax String ::= #getDatumAsString( VarLenDatum ) [function]
//-------------------------------------------------------------
  rule #getDatumAsString( VDat( _ , D ) ) => Int2String( D )

```

## Reading Terms

```k
  syntax Term ::= #readProgramTerm( K, BitStream ) [function]

  rule #readProgramTerm( #readTerm => #readTermTag #readNBits( #termTagLength, BitStream( I, BYTES) ),
                         BitStream( I => I +Int #termTagLength, BYTES )
                       )

  rule #readProgramTerm( #readTermTag DELAY, BITSTREAM ) => ( delay #readProgramTerm( #readTerm, BITSTREAM ) )

  rule #readProgramTerm( #readTermTag ERROR, _ ) => ( error )

  rule #readProgramTerm( #readTermTag CON => #readConType #readType( BitStream( I, Bs ) ),
                         BitStream( I => I +Int #typeLength, Bs )
                       )

  rule #readProgramTerm( #readConType UNIT, _ ) => ( con unit () )

  rule #readProgramTerm( #readConType BOOL, BitStream( I, BYTES) ) => ( con bool #bit2boolval( #readNBits( 1, BitStream( I, BYTES ) ) ) )

  rule #readProgramTerm( #readConType INTEGER, BitStream( I, BYTES) ) => ( con integer #getDatum(#readIntegerValue( BitStream( I, BYTES ) ) ) )

  rule #readProgramTerm( TERM:Term ~> ., _ ) => TERM
```

### Utility Functions Used to Read Terms

```k
  syntax Constant ::= #bit2boolval(Int) [function]
//------------------------------------------------
  rule #bit2boolval(0) => False
  rule #bit2boolval(1) => True

  // TODO: fix this function to support parametric types

  syntax Int ::= #readType( BitStream ) [function]
//------------------------------------------------
  rule #readType( BitStream( I, BYTES ) ) => #readNBits( 4, BitStream( I +Int 1, BYTES ) )

  syntax Version ::= String2Version (String) [function, functional, hook(STRING.string2token)]
```

### Values used to represent Terms and Types

#### Values for term tags

The following `Int`s represent different terms terms and encoded using 4 bits.

```k
  syntax Int ::= "#termTagLength" [macro]
//---------------------------------------
  rule #termTagLength => 4

  syntax Int ::= "VAR"     [macro]
               | "DELAY"   [macro]
               | "LAMBDA"  [macro]
               | "APP"     [macro]
               | "CON"     [macro]
               | "FORCE"   [macro]
               | "ERROR"   [macro]
               | "BUILTIN" [macro]
//--------------------------------
  rule VAR     => 0
  rule DELAY   => 1
  rule LAMBDA  => 2
  rule APP     => 3
  rule CON     => 4
  rule FORCE   => 5
  rule ERROR   => 6
  rule BUILTIN => 7
```

#### Values for type

The following `Int`s represent types within the untypted plutus language and encoded in 4 bits.
They are used as parameters that describe constants or as parameters to builtin functions.

```k
  syntax Int ::= "#typeLength" [macro]
//------------------------------------
  rule #typeLength => 6

  syntax Int ::= "INTEGER"    [macro]
               | "BYTESTRING" [macro]
               | "STRING"     [macro]
               | "UNIT"       [macro]
               | "BOOL"       [macro]
               | "LIST"       [macro]
               | "PAIR"       [macro]
               | "TYPE_APP"   [macro]
               | "DATA"       [macro]
//-----------------------------------
  rule INTEGER    => 0
  rule BYTESTRING => 1
  rule STRING     => 2
  rule UNIT       => 3
  rule BOOL       => 4
  rule LIST       => 5
  rule PAIR       => 6
  rule TYPE_APP   => 7
  rule DATA       => 8
```

### Variable Length Data

There are two parts to variable length data that are necessary for parsing: the actual datum and
the length of the datum. To facilitate parsing, the `VarLenDatum` sort has a datum that was parsed
and the bit length that was traversed when parsing the datum.

```k
  syntax VarLenDatum ::= VDat( BitLength:Int, Datum:Int )

  syntax Int ::= #getDatum( VarLenDatum ) [function]
//--------------------------------------------------
  rule #getDatum( VDat( _, D ) ) => D

  syntax Int ::= #getBitLength( VarLenDatum ) [function]
//------------------------------------------------------
  rule #getBitLength( VDat( BitLen, _ ) ) => BitLen

  syntax VarLenDatum ::= #getVarLenData( BitStream ) [function]
//-------------------------------------------------------------
  rule #getVarLenData( BITSTREAM ) => #getVarLenData( 0, 0, BITSTREAM )

  syntax VarLenDatum ::= #getVarLenData( CurrentByteOffset:Int, CurrentDatum:Int, BitStream ) [function]
//------------------------------------------------------------------------------------------------------
  rule #getVarLenData( I0 => I0 +Int 1,
                       D  => D +Int ( #readNBits( 7, BitStream( I1 +Int 1 , BYTES ) ) <<Int ( 7 *Int I0 ) ),
                       BitStream( I1, BYTES ) => BitStream( I1 +Int 8, BYTES )
                     )
    requires #readNBits( 1, BitStream( I1, BYTES ) ) ==Int 1

  rule #getVarLenData( I0, D, BitStream( I1, BYTES) ) =>
    VDat( ( I0 +Int 1 ) *Int 8, D +Int ( #readNBits( 7, BitStream( I1 +Int 1, BYTES ) ) <<Int ( 7 *Int I0 ) ) ) [owise]

```

### Reading Integer Values

```k
  syntax VarLenDatum ::= #readIntegerValue( BitStream ) [function]
//----------------------------------------------------------------
  rule #readIntegerValue( Bs ) => #readIntegerValue( #getVarLenData( Bs ) )

  syntax VarLenDatum ::= #readIntegerValue( VarLenDatum ) [function]
//------------------------------------------------------------------
  rule #readIntegerValue( VDat( Len, Dat ) ) => VDat( Len, #decodeZigZag( Dat ) )
```

This zigzag decoding algorithm is derived from this gist: https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba
which is also referenced by Haskell's `Data.ZigZag` library used in uplc.

```k
  syntax Int ::= #decodeZigZag( Int ) [function]
//----------------------------------------------
  rule #decodeZigZag( I ) => ( I >>Int 1 )
  requires I %Int 2 ==Int 0

  rule #decodeZigZag( I ) => (~Int I) >>Int 1 [owise]

```

```k
endmodule
```
