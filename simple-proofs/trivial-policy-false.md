# Verifying the Trivial Policy

```k
requires "verification.md"

module TRIVIAL-POLICY-FALSE
  imports VERIFICATION
```

## Decoder claims

```k

  // DataCredential:PubKeyCredential
  claim
    <k>
      ( con data { Constr 0 [ ByteString BS:ByteString , .DataList ] } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      DataCredential(0, BS) ... </k>
    <env> _ => .Map </env>

  // DataCredential:ScriptCredential
  claim
    <k>
      ( con data { Constr 1 [ ByteString BS:ByteString , .DataList ] } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      DataCredential(1, BS) ... </k>
    <env> _ => .Map </env>

  // DataCredential
  claim
    <k>
      ( con data { Constr I [ ByteString BS:ByteString , .DataList ] } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      DataCredential(I, BS) ... </k>
    <env> _ => .Map </env>
    requires DataCredentialDef(I)

  // StakingCredential:StakingHash
  claim
    <k>
      ( con data { Constr 0 [ Constr I [ ByteString BS:ByteString , .DataList ] ] } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingHash(I, BS) ... </k>
    <env> _ => .Map </env>
    requires DataCredentialDef(I)

  // StakingCredential:StakingPtr
  claim
    <k>
      ( con data { Constr 1 [ Integer I1, Integer I2, Integer I3 , .DataList ] } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingPtr(I1, I2, I3) ... </k>
    <env> _ => .Map </env>

  // TODO

  // TxId
  claim
    <k>
      ( con data { Constr 0 [ ByteString BS:ByteString , _ ] } ) ~>
      [ gLookup(fUnsafeFromDataTxId_cunsafeFromBuiltinData) _]
      =>
      < con bytestring BS > ... </k>
    <env> _ => .Map </env>

  // TxOutRef
  claim
    <k>
      ( con data { Constr 0 [ Constr 0 [ ByteString BS:ByteString , _ ] , Integer I:Int, _ ] } ) ~>
      [ gLookup(fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData) _]
      =>
      TxOut(BS, I)
      ... </k>
    <env> _ => .Map </env>

  // ScriptPurpose: Spending
  claim
    <k>
      ( con data { Constr 1 [ Constr 0 [ Constr 0 [ ByteString BS:ByteString , _ ] , Integer I:Int, _ ] , _ ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurposeSpending(BS, I)
      ... </k>
    <env> _ => .Map </env>

  // ScriptPurpose: Minting
  claim
    <k>
      ( con data { Constr 0 [ ByteString BS:ByteString , .DataList ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurposeMinting(BS)
      ... </k>
    <env> _ => .Map </env>
```

```k
endmodule
```