# Verifying the Trivial Policy

```k
requires "verification.md"

module TRIVIAL-POLICY-FALSE
  imports VERIFICATION
```

## Decoder claims

### Credential

```
  // Constructor: 0: PubKeyCredential [151/115 steps]
  claim
    <k>
      ( con data { CredentialData(0, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      Credential(0, ListItem(BS:ByteString)) ... </k>
    <env> _ => .Map </env>

  // Constructor: 1: ScriptCredential [99/84 steps]
  claim
    <k>
      ( con data { CredentialData(1, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      Credential(1, ListItem(BS:ByteString)) ... </k>
    <env> _ => .Map </env>
```

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { CredentialData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      Credential(C, PARAMS) ... </k>
    <env> _ => .Map </env>
    requires CredentialDef(C, PARAMS)
    [trusted]
```

### Staking Credential

```
  // StakingCredential: 0: StakingHash [157/121 steps]
  claim
    <k>
      ( con data { Constr 0 [ CredentialData(C, PARAMS) ] } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingCredential(0, ListItem(ListItem(C) ListItem(PARAMS))) ... </k>
    <env> _ => .Map </env>
    requires CredentialDef(C, PARAMS)

  // StakingCredential: 1: StakingPtr [215/155 steps]
  claim
    <k>
      ( con data { Constr 1 [ Integer I1, Integer I2, Integer I3 ] } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingCredential(1, ListItem(I1) ListItem(I2) ListItem(I3)) ... </k>
    <env> _ => .Map </env>
```

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { StakingCredentialData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingCredential(C, PARAMS) ... </k>
    <env> _ => .Map </env>
    requires StakingCredentialDef(C, PARAMS)
    [trusted]
```

### TxId

```
  // TxId: 0 [90/78 steps]
  claim
    <k>
      ( con data { Constr 0 [ ByteString BS ] } ) ~>
      [ gLookup(fUnsafeFromDataTxId_cunsafeFromBuiltinData) _]
      =>
      TxId(0, ListItem(BS)) ... </k>
    <env> _ => .Map </env>
```

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { TxIdData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataTxId_cunsafeFromBuiltinData) _]
      =>
      TxId(C, PARAMS) ... </k>
    <env> _ => .Map </env>
    requires TxIdDef(C, PARAMS)
    [trusted]
```

### TxOutRef

```
  // TxOutRef: 0 [145/109 steps]
  claim
    <k>
      ( con data { Constr 0 [ TxIdData(C, PARAMS), Integer I ] } ) ~>
      [ gLookup(fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData) _]
      =>
      TxOutRef(0, ListItem(ListItem(C) ListItem(PARAMS)) ListItem(I)) ... </k>
    <env> _ => .Map </env>
    requires TxIdDef(C, PARAMS)
```

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { TxOutRefData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData) _]
      =>
      TxOutRef(C, PARAMS) ... </k>
    <env> _ => .Map </env>
    requires TxOutRefDef(C, PARAMS)
    [trusted]
```

### DCert

TODO: WRITE PER-CONSTRUCTOR CLAIMS

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { DCertData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(C, PARAMS) ... </k>
    <env> _ => .Map </env>
    requires DCertDef(C, PARAMS)
    [trusted]
```

### ScriptPurpose

```
  // ScriptPurpose: 0: Minting [243/165 steps]
  claim
    <k>
      ( con data { Constr 0 [ ByteString BS:ByteString ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(0, ListItem(BS))
      ... </k>
    <env> _ => .Map </env>

  // ScriptPurpose: 1: Spending [188/131 steps]
  claim
    <k>
      ( con data { Constr 1 [ TxOutRefData(C, PARAMS) ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(1, ListItem(ListItem(ListItem(C) ListItem(PARAMS))))
      ... </k>
    <env> _ => .Map </env>
    requires TxOutRefDef(C, PARAMS)

  // ScriptPurpose: 2: Rewarding [141/105 steps]
  claim
    <k>
      ( con data { Constr 2 [ StakingCredentialData(C, PARAMS) ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(2, ListItem(ListItem(ListItem(C) ListItem(PARAMS))))
      ... </k>
    <env> _ => .Map </env>
    requires StakingCredentialDef(C, PARAMS)

  // ScriptPurpose: 3: Certifying [94/79 steps]
  claim
    <k>
      ( con data { Constr 3 [ DCertData(C, PARAMS) ] } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(3, ListItem(ListItem(ListItem(C) ListItem(PARAMS))))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(C, PARAMS)
```

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { ScriptPurposeData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(C, PARAMS)
      ... </k>
    <env> _ => .Map </env>
    requires ScriptPurposeDef(C, PARAMS)
    [trusted]
```

```k
endmodule
```