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

  rule #bytes2program( BYTES ) => #bytes2program( #readVersion( BitStream( 0, BYTES ) ),
                                                  #readProgramTerm( #readTerm, BitStream( 24, BYTES ) )
                                                )

  rule #bytes2program( VERSION, TERM:Term ~> .) => ( program String2Version( VERSION ) TERM )
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
  syntax String ::= #readVersion(BitStream) [function]
//----------------------------------------------------
  rule #readVersion( BitStream( I, BYTES ) ) =>
    Int2String( #readNBits( 8, BitStream( I, BYTES         ) ) ) +String "." +String
    Int2String( #readNBits( 8, BitStream( I +Int 8,  BYTES ) ) ) +String "." +String
    Int2String( #readNBits( 8, BitStream( I +Int 16, BYTES ) ) )
```

## Reading Terms Term

```k
  syntax Term ::= #readProgramTerm( K, BitStream ) [function]

  rule #readProgramTerm( #readTerm => #readTermTag #readNBits( 4, BitStream( 24, BYTES) ),
                         BitStream( I => I +Int 4, BYTES )
                       )

  rule #readProgramTerm( #readTermTag ERROR => ( error ),
                         BitStream( I => #nextByteBoundary(I), _ )
                       )

  rule #readProgramTerm( #readTermTag CON => #readConType #readType( BitStream( I, Bs ) ),
                         BitStream( I => I +Int  6, Bs )
                       )

  rule #readProgramTerm( #readConType UNIT  => ( con unit () ),
                         BitStream( I => #nextByteBoundary( I ), _ )
                       )

  rule #readProgramTerm( #readConType BOOL => ( con bool #bit2boolval( #readNBits( 1, BitStream( I, Bs ) ) ) ),
                         BitStream( I => #nextByteBoundary( I ), Bs )
                       )

  rule #readProgramTerm( TERM:Term ~> ., BitStream( X, BYTES ) ) => TERM
    requires (X /Int 8) ==Int lengthBytes( BYTES )

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

endmodule
```
