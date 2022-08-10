# Verification of trivial policies

```k
requires "verification.md"

module DATATYPE-ABSTRACTIONS
  imports VERIFICATION
```

## Decoder claims

### Credential

```
  // Constructor: 0: PubKeyCredential [151 steps]
  claim
    <k>
      ( con data { CredentialData(0, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      Credential(0, ListItem(BS)) ... </k>
    <env> _ => .Map </env>
    requires CredentialDef(0, ListItem(BS))

  // Constructor: 1: ScriptCredential [99 steps]
  claim
    <k>
      ( con data { CredentialData(1, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataCredential_cunsafeFromBuiltinData) _]
      =>
      Credential(1, ListItem(BS)) ... </k>
    <env> _ => .Map </env>
    requires CredentialDef(0, ListItem(BS))
```

```
  rule { true #Equals CredentialDef(C, PARAMS) } =>
       ({ C #Equals 0 } #And (#Exists BS:ByteString. { PARAMS #Equals ListItem(BS) }) ) #Or
       ({ C #Equals 1 } #And (#Exists BS:ByteString. { PARAMS #Equals ListItem(BS) }) ) [simplification, unboundVariables(BS) ]
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
  // StakingCredential: 0: StakingHash [157 steps]
  claim
    <k>
      ( con data { StakingCredentialData(0, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingCredential(0, ListItem(ListItem(C) ListItem(PARAMS))) ... </k>
    <env> _ => .Map </env>
    requires StakingCredentialDef(0, ListItem(ListItem(C) ListItem(PARAMS)))

  // StakingCredential: 1: StakingPtr [215 steps]
  claim
    <k>
      ( con data { StakingCredentialData(1, ListItem(I1:Int) ListItem(I2:Int) ListItem(I3:Int)) } ) ~>
      [ gLookup(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData) _]
      =>
      StakingCredential(1, ListItem(I1) ListItem(I2) ListItem(I3)) ... </k>
    <env> _ => .Map </env>
    requires StakingCredentialDef(1, ListItem(I1) ListItem(I2) ListItem(I3))
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
  // TxId: 0 [90 steps]
  claim
    <k>
      ( con data { TxIdData(0, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataTxId_cunsafeFromBuiltinData) _]
      =>
      TxId(0, ListItem(BS)) ... </k>
    <env> _ => .Map </env>
    requires TxIdDef(0, ListItem(BS))
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
  // TxOutRef: 0 [145 steps]
  claim
    <k>
      ( con data { TxOutRefData(0, ListItem(ListItem(C:Int) ListItem(PARAMS:List)) ListItem(I:Int)) } ) ~>
      [ gLookup(fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData) _]
      =>
      TxOutRef(0, ListItem(ListItem(C) ListItem(PARAMS)) ListItem(I)) ... </k>
    <env> _ => .Map </env>
    requires TxOutRefDef(0, ListItem(ListItem(C) ListItem(PARAMS)) ListItem(I))
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

```
  // DCert: 0: DCertDelegRegKey [395 steps]
  claim
    <k>
      ( con data { DCertData(0, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(0, ListItem(ListItem(C:Int) ListItem(PARAMS)))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(0, ListItem(ListItem(C:Int) ListItem(PARAMS)))

  // DCert: 1: DCertDelegDeRegKey [343 steps]
  claim
    <k>
      ( con data { DCertData(1, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(1, ListItem(ListItem(C:Int) ListItem(PARAMS)))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(1, ListItem(ListItem(C:Int) ListItem(PARAMS)))

  // DCert: 2: DCertDelegDelegate [345 steps]
  claim
    <k>
      ( con data { DCertData(2, ListItem(ListItem(C:Int) ListItem(PARAMS:List)) ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(2, ListItem(ListItem(C:Int) ListItem(PARAMS)) ListItem(BS:ByteString))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(2, ListItem(ListItem(C:Int) ListItem(PARAMS)) ListItem(BS:ByteString))

  // DCert: 3: DCertPoolRegister [301 steps]
  claim
    <k>
      ( con data { DCertData(3, ListItem(BS1:ByteString) ListItem(BS2:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(3, ListItem(BS1:ByteString) ListItem(BS2:ByteString))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(3, ListItem(BS1:ByteString) ListItem(BS2:ByteString))

  // DCert: 4: DCertPoolRetire [254 steps]
  claim
    <k>
      ( con data { DCertData(4, ListItem(BS:ByteString) ListItem(I:Int)) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(4, ListItem(BS:ByteString) ListItem(I:Int))
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(4, ListItem(BS:ByteString) ListItem(I:Int))

  // DCert: 5: DCertGenesis [117 steps]
  claim
    <k>
      ( con data { DCertData(5, .List) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(5, .List)
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(5, .List)

  // DCert: 6: DCertMir [70 steps]
  claim
    <k>
      ( con data { DCertData(6, .List) } ) ~>
      [ gLookup(fUnsafeFromDataDCert_cunsafeFromBuiltinData) _]
      =>
      DCert(6, .List)
      ... </k>
    <env> _ => .Map </env>
    requires DCertDef(6, .List)
```

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
  // ScriptPurpose: 0: Minting [243 steps]
  claim
    <k>
      ( con data { ScriptPurposeData(0, ListItem(BS:ByteString)) } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(0, ListItem(BS))
      ... </k>
    <env> _ => .Map </env>
    requires ScriptPurposeDef(0, ListItem(BS))

  // ScriptPurpose: 1: Spending [188 steps]
  claim
    <k>
      ( con data { ScriptPurposeData(1, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(1, ListItem(ListItem(C) ListItem(PARAMS)))
      ... </k>
    <env> _ => .Map </env>
    requires ScriptPurposeDef(1, ListItem(ListItem(C) ListItem(PARAMS)))

  // ScriptPurpose: 2: Rewarding [141 steps]
  claim
    <k>
      ( con data { ScriptPurposeData(2, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(2, ListItem(ListItem(C) ListItem(PARAMS)))
      ... </k>
    <env> _ => .Map </env>
    requires ScriptPurposeDef(2, ListItem(ListItem(C) ListItem(PARAMS)))

  // ScriptPurpose: 3: Certifying [94 steps]
  claim
    <k>
      ( con data { ScriptPurposeData(3, ListItem(ListItem(C:Int) ListItem(PARAMS:List))) } ) ~>
      [ gLookup(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData) _]
      =>
      ScriptPurpose(3, ListItem(ListItem(C) ListItem(PARAMS)))
      ... </k>
    <env> _ => .Map </env>
    requires ScriptPurposeDef(3, ListItem(ListItem(C) ListItem(PARAMS)))
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

### TxInfo: TODO

```k
  // General claim [??? steps]
  claim
    <k>
      ( con data { TxInfoData(C, PARAMS) } ) ~>
      [ gLookup(fUnsafeFromDataTxInfo_cunsafeFromBuiltinData) _]
      =>
      TxInfo(C, PARAMS)
      ... </k>
    <env> _ => .Map </env>
    requires TxInfoDef(C, PARAMS)
    [trusted]
```

## Trivial policy: True

```k
  // True trivial policy returns the delayed identity
  claim
    <k>
        [
          TRIVIAL_POLICY_TRUE
          (con data { Constr 0 [ _ ] } )
          (con data { Constr 0 [ TxInfoData(C1, PARAMS1), ScriptPurposeData(C2, PARAMS2) ] })
        ]
      =>  < delay ( lam case_Unit case_Unit ) .Map >
    ... </k>
    <env> _ => .Map </env>
    requires TxInfoDef(C1, PARAMS1)
     andBool ScriptPurposeDef(C2, PARAMS2)
```

## Trivial policy: False

```k
  // False trivial policy throws an error
  claim
    <k>
        [
          TRIVIAL_POLICY_FALSE
          (con data { Constr 0 [ _ ] } )
          (con data { Constr 0 [ TxInfoData(C1, PARAMS1), ScriptPurposeData(C2, PARAMS2) ] })
        ]
      => (error)
    ... </k>
    <env> _ => ?_ </env>
    <trace> TRACE => TRACE ListItem("PT5") </trace>
    requires TxInfoDef(C1, PARAMS1)
     andBool ScriptPurposeDef(C2, PARAMS2)
```

```k
endmodule
```