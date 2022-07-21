# BitStreams

This module defines a `BitStream` sort. This sort can be used to get arbitrary numbers of bits from
a `Bytes` sort. This use an integer that represents the current position to "traverse" the `Bytes`.
This allows users to operate on this sort without having to keep track of the current position.
However, they are expected to update this number after using operations such as #readNBits.

```k
module BITSTREAM
  imports BYTES
  imports INT

  syntax BitStream ::= BitStream(Int, Bytes)

  syntax Bytes ::= getBitStreamBytes(BitStream) [function]
//--------------------------------------------------------
  rule getBitStreamBytes( BitStream(_, Bs) ) => Bs

  syntax Int ::= getBitStreamPos(BitStream) [function]
//----------------------------------------------------
  rule getBitStreamPos( BitStream(I, _) ) => I


  syntax Int ::= indexBitStream (Int, BitStream) [function]
//---------------------------------------------------------
  rule indexBitStream(I, Bs) => (getBitStreamBytes(Bs)[I divInt 8] >>Int (7 -Int (I modInt 8))) &Int 1 


  syntax Int ::= getNextBit(BitStream) [function]
//-----------------------------------------------
  rule getNextBit(Bs) => indexBitStream(getBitStreamPos(Bs), Bs)

  syntax BitStream ::= advancePos(BitStream) [function]
//-----------------------------------------------------
  rule advancePos(Bs) => BitStream(getBitStreamPos(Bs) +Int 1, getBitStreamBytes(Bs))

  syntax BitStream ::= #advancePosNBits(Int, BitStream) [function]
//----------------------------------------------------------------
  rule #advancePosNBits(N, Bs) => BitStream(getBitStreamPos(Bs) +Int N, getBitStreamBytes(Bs))

  syntax Int ::= #readNBits(Int, BitStream) [function]
//----------------------------------------------------
  rule #readNBits(0,  _) => 0
  rule #readNBits(N, Bs) => (getNextBit(Bs) <<Int N -Int 1) |Int #readNBits(N -Int 1, advancePos(Bs)) [owise]

  syntax Int ::= #nextByteBoundary(Int) [function]
//------------------------------------------------
  rule #nextByteBoundary(I) => I +Int ( 8 -Int ( I modInt 8 ) )

endmodule
```
