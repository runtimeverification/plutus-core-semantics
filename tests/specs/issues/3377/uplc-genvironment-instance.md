# Global Environment Instance for the Oracle Contract

## Dependencies and inclusions

```k
require "uplc-genvironment.md"
require "uplc-configuration.md"
require "uplc-rw-helpers.md"
require "uplc-cbor-parser.md"

module UPLC-GENVIRONMENT-INSTANCE
  imports SET
  imports UPLC-BYTESTRING
  imports UPLC-CBOR-PARSER
  imports UPLC-DATA-BUILTINS
  imports UPLC-ID
  imports UPLC-MAP
  imports UPLC-RW-HELPERS
  imports UPLC-GENVIRONMENT
  imports UPLC-CONFIGURATION

  syntax ByteString
```

## Global environment keys

```k
  syntax Set ::= "GENV_KEYS" [macro]
  rule GENV_KEYS =>
    SetItem(k_fix:UplcId)

    SetItem(k_True:UplcId)
    SetItem(k_False:UplcId)
    SetItem(k_Bool_match:UplcId)

    SetItem(k_Nil:UplcId)
    SetItem(k_Cons:UplcId)
    SetItem(k_Nil_match:UplcId)

    SetItem(k_CustomTxInInfo:UplcId)
    SetItem(k_CustomTxInInfo_match:UplcId)

    SetItem(k_CustomTxOut:UplcId)
    SetItem(k_CustomTxOut_match:UplcId)

    SetItem(k_Just:UplcId)
    SetItem(k_Nothing:UplcId)
    SetItem(k_Maybe_match:UplcId)

    SetItem(k_toScriptCredential:UplcId)
```

### Global environment domain
```k
  rule #inKeysgEnv(X) => X in GENV_KEYS
```

## Function-related aliases

### k_noScriptInInput

```k
  syntax Term ::= "NO_SCRIPT_IN_INPUT" [alias]
  rule NO_SCRIPT_IN_INPUT =>
    (lam k_scrHash (lam k_r_inputs NO_SCRIPT_IN_INPUT_CORE))

  syntax Term ::= "NO_SCRIPT_IN_INPUT_CORE" [alias]
  rule NO_SCRIPT_IN_INPUT_CORE =>
    [ [ (force (force k_fix)) (lam k_aux_go (lam k_ds_438 GO_NO_SCRIPT_IN_INPUT_CORE ) ) ] k_r_inputs ]

  syntax Term ::= "GO_NO_SCRIPT_IN_INPUT_CORE" [alias]
  rule GO_NO_SCRIPT_IN_INPUT_CORE =>
    (force [ [ (force [ (force k_Nil_match) k_ds_438 ] ) (delay k_True) ] (lam k_ds_439 (lam tl_440 (delay [ (force [ k_CustomTxInInfo_match k_ds_439 ] ) (lam k_ds_441 (lam k_ds_442 [ (force [ k_CustomTxOut_match k_ds_442 ] ) (lam r_addr_443 (lam k_ds_444 (lam k_ds_445 (force [ [ (force [ (force k_Maybe_match) [ k_toScriptCredential r_addr_443 ] ] ) (lam sh_446 (delay (force [ [ (force [ k_Bool_match [ [ [ (force (builtin ifThenElse)) [ [ (builtin equalsByteString) sh_446 ] k_scrHash ] ] k_True ] k_False ] ] ) (delay k_False) ] (delay [ k_aux_go tl_440 ] ) ] ) ) ) ] (delay [ k_aux_go tl_440 ] ) ] ) ) ) ) ] ) ) ] ) ) ) ] )
```

## Datatype Abstractions

The datatypes used by the Oracle contract need to come with associated abstractions, which capture the shape of the lambda-terms representing the type inhabitants as well as their corresponding data encodings.

### Bool

```k
  // Datatype
  syntax Value ::= Bool(Int, ConstantList) [function, injective]
  rule Bool(0, L) => BFalse(L) [simplification]
  rule Bool(1, L) => BTrue(L) [simplification]

  syntax Bool ::= BoolDef(Int, ConstantList) [function, total]
  rule BoolDef(0, L) => BFalseDef(L)
  rule BoolDef(1, L) => BTrueDef(L)
  rule BoolDef(_, _) => false [owise]

  rule #Ceil(Bool(C, PARAMS)) => { true #Equals BoolDef(C, PARAMS) } [simplification]

  // Constructor 0: BFalse
  syntax Value ::= BFalse(ConstantList) [function, injective]
  rule BFalse(.ConstantList) =>
    < delay (lam k_case_True (lam k_case_False k_case_False)) .Map >

  syntax Bool ::= BFalseDef(ConstantList) [function, total]
  rule BFalseDef(PARAMS) => { PARAMS #Equals .ConstantList }

  rule #Ceil(BFalse(PARAMS)) => BFalseDef(PARAMS) [simplification]

  // Constructor 1: BTrue
  syntax Value ::= BTrue(ConstantList) [function, injective]
  rule BTrue(.ConstantList) =>
    < delay (lam k_case_True (lam k_case_False k_case_True)) .Map >

  syntax Bool ::= BTrueDef(ConstantList) [function, total]
  rule BTrueDef(PARAMS) => { PARAMS #Equals .ConstantList }

  rule #Ceil(BTrue(PARAMS)) => BTrueDef(PARAMS) [simplification]

  // Textual Data
  syntax TextualData ::= BoolData(Int, ConstantList) [function, injective]
  rule BoolData(0, .ConstantList) => Constr 0 [ .DataList ]
  rule BoolData(1, .ConstantList) => Constr 1 [ .DataList ]

  rule #Ceil(BoolData(C, PARAMS)) => { true #Equals BoolDef(C, PARAMS) } [simplification]
```

### <a name="Credential"></a> Credential

```k
  // Datatype
  syntax Value ::= Credential(Int, ConstantList) [function, injective]
  rule Credential(0, L) => PubKeyCredential(L) [simplification]
  rule Credential(1, L) => ScriptCredential(L) [simplification]

  syntax Bool ::= CredentialDef(Int, ConstantList) [function, total]
  rule CredentialDef(0, L) => PubKeyCredentialDef(L)
  rule CredentialDef(1, L) => ScriptCredentialDef(L)
  rule CredentialDef(_, _) => false [owise]

  rule #Ceil(Credential(C, PARAMS)) =>
    ({ C #Equals 0 } #And (#Exists CBS:ByteString. { PARAMS #Equals CBS }) ) #Or
    ({ C #Equals 1 } #And (#Exists CBS:ByteString. { PARAMS #Equals CBS }) )
    [simplification]

  // Constructor 0: PubKeyCredential
  syntax Value ::= PubKeyCredential(ConstantList) [function, injective]
  rule PubKeyCredential(BS:ByteString) =>
    < delay
      (lam k_case_PubKeyCredential (lam k_case_ScriptCredential [ k_case_PubKeyCredential k_arg_0 ]))
      k_arg_0 |-> < con bytestring BS >
    >

  syntax Bool ::= PubKeyCredentialDef(ConstantList) [function, total]
  rule PubKeyCredentialDef(_:ByteString) => true
  rule PubKeyCredentialDef(_) => false [owise]

  rule #Ceil(PubKeyCredential(PARAMS)) => { true #Equals PubKeyCredentialDef(PARAMS) } [simplification]

  // Constructor 1: ScriptCredential
  syntax Value ::= ScriptCredential(ConstantList) [function, injective]
  rule ScriptCredential(BS:ByteString) =>
    < delay
      (lam k_case_PubKeyCredential (lam k_case_ScriptCredential [ k_case_ScriptCredential k_arg_0 ]))
      k_arg_0 |-> < con bytestring BS >
    >

  syntax Bool ::= ScriptCredentialDef(ConstantList) [function, total]
  rule ScriptCredentialDef(_:ByteString) => true
  rule ScriptCredentialDef(_) => false [owise]

  rule #Ceil(ScriptCredential(PARAMS)) => { true #Equals ScriptCredentialDef(PARAMS) } [simplification]

  // Textual Data
  syntax TextualData ::= CredentialData(Int, ConstantList) [function, injective]
  rule CredentialData(0, BS:ByteString) => Constr 0 [ ByteString BS ]
  rule CredentialData(1, BS:ByteString) => Constr 1 [ ByteString BS ]

  rule #Ceil(CredentialData(C, PARAMS)) => #Ceil(CredentialDef(C, PARAMS)) [simplification]
```

### Staking Credential

```k
  // Datatype
  syntax Value ::= StakingCredential(Int, ConstantList) [function, injective]
  rule StakingCredential(0, L) => StakingHash(L)
  rule StakingCredential(1, L) => StakingPtr(L)

  syntax Bool ::= StakingCredentialDef(Int, ConstantList) [function, total]
  rule StakingCredentialDef(0, L) => StakingHashDef(L)
  rule StakingCredentialDef(1, L) => StakingPtrDef(L)
  rule StakingCredentialDef(_, _) => false [owise]

  rule #Ceil(StakingCredential(C, PARAMS)) => { true #Equals StakingCredentialDef(C, PARAMS) } [simplification]

  // Constructor 0: StakingHash
  syntax Value ::= StakingHash(ConstantList) [function, injective]
  rule StakingHash(C:Int, PARAMS:ConstantList) =>
    < delay
      (lam k_case_StakingHash (lam k_case_StakingPtr [ k_case_StakingHash k_arg_0 ]))
      k_arg_0 |-> Credential(C, PARAMS)
    >

  syntax Bool ::= StakingHashDef(ConstantList) [function, total]
  rule StakingHashDef([ C:Int, PARAMS:ConstantList ]) => CredentialDef(C, PARAMS)
  rule StakingHashDef(_) => false [owise]

  rule #Ceil(StakingHash(PARAMS)) => { true #Equals StakingHashDef(PARAMS) } [simplification]

  // Constructor 1: StakingPtr
  syntax Value ::= StakingPtr(ConstantList) [function, injective]
  rule StakingPtr(I1:Int, I2:Int, I3:Int) =>
    < delay
      (lam k_case_StakingHash (lam k_case_StakingPtr [ k_case_StakingPtr k_arg_0 k_arg_1 k_arg_2 ]))
      k_arg_0 |-> < con integer I1 >
      k_arg_1 |-> < con integer I2 >
      k_arg_2 |-> < con integer I3 >
    >

  syntax Bool ::= StakingPtrDef(ConstantList) [function, total]
  rule StakingPtrDef(_:Int, _:Int, _:Int) => true
  rule StakingPtrDef(_) => false [owise]

  rule #Ceil(StakingPtr(PARAMS)) => { true #Equals StakingPtrDef(PARAMS) } [simplification]

  // Textual Data
  syntax TextualData ::= StakingCredentialData(Int, ConstantList) [function, injective]
  rule StakingCredentialData(0, [ C:Int, PARAMS:ConstantList ]) =>
    Constr 0 [ CredentialData(C, PARAMS) ]
  rule StakingCredentialData(1, (I1:Int, I2:Int, I3:Int)) =>
    Constr 1 [ Integer I1, Integer I2, Integer I3 ]

  rule #Ceil(StakingCredentialData(C, PARAMS)) => { true #Equals StakingCredentialDef(C, PARAMS) } [simplification]
```

### TxId

```k
  // Datatype
  syntax Value ::= TxId(ConstantList) [function, injective]
  rule TxId(BS:ByteString) => < con bytestring BS >

  syntax Bool ::= TxIdDef(ConstantList) [function, total]
  rule TxIdDef(_:ByteString) => true
  rule TxIdDef(_) => false [owise]

  rule #Ceil(TxId(PARAMS)) => { true #Equals TxIdDef(PARAMS) } [simplification]

  // Textual Data
  syntax TextualData ::= TxIdData(ConstantList) [function, injective]
  rule TxIdData(BS:ByteString) => Constr 0 [ ByteString BS ]

  rule #Ceil(TxIdData(C, PARAMS)) => { true #Equals TxIdDef(C, PARAMS) } [simplification]
```

### TxOutRef

```k
  // Datatype
  syntax Value ::= TxOutRef(ConstantList) [function, injective]
  rule TxOutRef([ PARAMS:ConstantList ], I:Int) =>
    < delay
      (lam k_case_TxOutRef [ k_case_TxOutRef k_arg_0 k_arg_1 ])
      k_arg_0 |-> TxId(PARAMS)
      k_arg_1 |-> < con integer I >
    >

  syntax Bool ::= TxOutRefDef(ConstantList) [function, total]
  rule TxOutRefDef([ PARAMS:ConstantList ], _:Int) => TxIdDef(PARAMS)
  rule TxOutRefDef(_) => false [owise]

  rule #Ceil(TxOutRef(PARAMS)) => { true #Equals TxOutRefDef(PARAMS) } [simplification]

  // Textual Data
  syntax TextualData ::= TxOutRefData(ConstantList) [function, injective]
  rule TxOutRefData([ PARAMS:ConstantList ], I:Int) =>
    Constr 0 [ TxIdData(PARAMS), Integer I ]

  rule #Ceil(TxOutRefData(PARAMS)) => { true #Equals TxOutRefDef(PARAMS) } [simplification]
```

### Address

```k
  // Textual Data
  syntax TextualData ::= AddressData(ConstantList) [function, injective]
  rule AddressData([ CC:Int, PC:ConstantList ], [ CSC:Int, PSC:ConstantList ]) =>
    Constr 0 [ CredentialData(CC, PC), MaybeData("StakingCredential", CSC, PSC) ]
```
```k
  rule #Ceil(AddressData(PARAMS)) =>
    #Exists ACC:Int.  #Exists APC:ConstantList.
    #Exists ACSC:Int. #Exists APSC:ConstantList.
      { PARAMS #Equals [ ACC:Int, APC:ConstantList ], [ ACSC:Int, APSC:ConstantList ] }
      #And #Ceil(Credential(ACC, APC))
      #And #Ceil(Maybe("StakingCredential", ACSC, APSC))
      [simplification]
```

### CustomTxOut

```k
  // Datatype
  syntax Value ::= CustomTxOut(ConstantList) [function, injective]
  // WARNING: The datatypes of the textual data are not precise at the moment, they may need refinement.
  //          In particular, OV is a Value, ODH is a Maybe DatumHash (Maybe ByteString).
  //          However, the decoder is not performing the decoding, so...
  rule CustomTxOut([ PA:ConstantList ], { OV:TextualData }, { OMDH:TextualData }) =>
    < delay
      (lam k_case_CustomTxOut [ [ [ k_case_CustomTxOut k_arg_0 ] k_arg_1 ] k_arg_2 ])
      k_arg_0 |-> < con data { AddressData(PA) } >
      k_arg_1 |-> < con data { OV } >
      k_arg_2 |-> < con data { OMDH } >
    >

  rule #Ceil(CustomTxOut(PARAMS)) =>
    #Exists PA:ConstantList. #Exists OV:TextualData. #Exists OMDH:TextualData.
      { PARAMS #Equals ([ PA:ConstantList ], { OV }, { OMDH }) }
      #And #Ceil(AddressData(PA)) [simplification]

  // Textual Data
  syntax TextualData ::= CustomTxOutData(ConstantList) [function, injective]
  rule CustomTxOutData([ PA:ConstantList ], { OV:TextualData }, { OMDH:TextualData }) =>
    Constr 0 [ AddressData(PA), OV, OMDH ]

  rule #Ceil(CustomTxOutData(PARAMS)) => #Ceil(CustomTxOut(PARAMS)) [simplification]
```

### CustomTxInInfo

```k
  // Datatype
  syntax Value ::= CustomTxInInfo(ConstantList) [function, injective]
  rule CustomTxInInfo([ PARAMS_OR:ConstantList ], [ PARAMS_O:ConstantList ]) =>
    < delay
      (lam k_case_CustomTxInInfo [ [ k_case_CustomTxInInfo k_arg_0 ] k_arg_1 ])
      k_arg_0 |-> < con data { TxOutRefData(PARAMS_OR) } >
      k_arg_1 |-> CustomTxOut(PARAMS_O)
    >

  rule #Ceil(CustomTxInInfo(PARAMS)) =>
    #Exists PARAMS_OR:ConstantList. #Exists PARAMS_O:ConstantList.
      { PARAMS #Equals [ PARAMS_OR:ConstantList ], [ PARAMS_O:ConstantList ] }
      #And #Ceil(TxOutRef(PARAMS_OR))
      #And #Ceil(CustomTxOut(PARAMS_O)) [simplification]

  // Textual Data
  syntax TextualData ::= CustomTxInInfoData(ConstantList) [function, injective]
  rule CustomTxInInfoData([ PARAMS_OR:ConstantList ], [ PARAMS_O:ConstantList ]) =>
    Constr 0 [ TxOutRefData(PARAMS_OR), CustomTxOutData(PARAMS_O) ]

  rule #Ceil(CustomTxInInfoData(PARAMS)) => #Ceil(CustomTxInInfo(PARAMS)) [simplification]
```

### DList

```k
  // Datatype
  syntax Value ::= DList(String, ConstantList) [function, injective]
  rule DList(T, .ConstantList) => Nil(T, .ConstantList)
  rule DList(T, C:Constant, CL:ConstantList) => Cons(T, C, CL)

  // Constructor 0: Nil
  syntax Value ::= Nil(String, ConstantList) [function]
  rule Nil("Data", .ConstantList) => < delay (lam k_case_Nil (lam k_case_Cons k_case_Nil)) .Map >
  rule Nil("CustomTxInInfo", .ConstantList) => < delay (lam k_case_Nil (lam k_case_Cons k_case_Nil)) .Map >

  rule #Ceil(Nil("Data", PARAMS)) => PARAMS ==K .ConstantList [simplification]
  rule #Ceil(Nil("CustomTxInInfo", PARAMS)) => PARAMS ==K .ConstantList [simplification]

  // Constructor 1: Cons
  syntax Value ::= Cons(String, ConstantList) [function, injective]
  rule Cons("Data", { LH:TextualData }, LT:ConstantList) =>
    < delay
      (lam k_case_Nil (lam k_case_Cons [ [ k_case_Cons k_arg_0 ] k_arg_1 ]))
      k_arg_0 |-> < con data { LH } >
      k_arg_1 |-> DList("Data", LT)
    >

  rule Cons("CustomTxInInfo", [ PC:ConstantList ], LT:ConstantList) =>
    < delay
      (lam k_case_Nil (lam k_case_Cons [ [ k_case_Cons k_arg_0 ] k_arg_1 ]))
      k_arg_0 |-> CustomTxInInfo(PC)
      k_arg_1 |-> DList("CustomTxInInfo", LT)
    >

  rule #Ceil(Cons("Data", PARAMS)) =>
    #Exists TD:TextualData. #Exists CL:ConstantList.
      { PARAMS #Equals ({ TD }, CL) } #And #Ceil(DList("Data", CL))
        [simplification]

  rule #Ceil(Cons("CustomTxInInfo", PARAMS)) =>
    #Exists PCX:ConstantList. #Exists CLX:ConstantList.
      { PARAMS #Equals [ PCX ], CLX } #And #Ceil(CustomTxInInfo(PCX)) #And #Ceil(DList("CustomTxInInfo", CLX))
        [simplification]

  // Textual Data
  syntax TextualData ::= DListData(String, ConstantList) [function, injective]
  rule DListData(T, PARAMS) => List [ DListDataDL(T, PARAMS) ] [simplification]

  rule #Ceil(DListData(T, PARAMS)) => #Ceil(DList(T, PARAMS)) [simplification]

  // Textual DataList
  syntax DataList ::= DListDataDL(String, ConstantList) [function, injective]
  rule DListDataDL("CustomTxInInfo", .ConstantList) => .DataList [simplification]
  rule DListDataDL("CustomTxInInfo", [ PH ]:Constant, PTL:ConstantList) =>
         CustomTxInInfoData(PH), DListDataDL("CustomTxInInfo", PTL) [simplification]

  rule #Ceil(DListDataDL(T, PARAMS)) => #Ceil(DList(T, PARAMS)) [simplification]

  // Textual ConstantList
  syntax ConstantList ::= DListDataCL(String, ConstantList) [function, injective]
  rule DListDataCL("CustomTxInInfo", .ConstantList) => .ConstantList [simplification]
  rule DListDataCL("CustomTxInInfo", [ PH ]:Constant, PTL:ConstantList) =>
         { CustomTxInInfoData(PH) }, DListDataCL("CustomTxInInfo", PTL) [simplification]

  rule #Ceil(DListDataCL(T, PARAMS)) => #Ceil(DList(T, PARAMS)) [simplification]
```

### Maybe

```k
  // Datatype
  syntax Value ::= Maybe(String, Int, ConstantList) [function, injective]
  rule Maybe(T, 0, L) => Just(T, L) [simplification]
  rule Maybe(T, 1, L) => Nothing(T, L) [simplification]

  syntax Bool ::= MaybeDef(String, Int, ConstantList) [function, total]
  rule MaybeDef(T, 0, L) => JustDef(T, L)
  rule MaybeDef(T, 1, L) => NothingDef(T, L)
  rule MaybeDef(_, _, _) => false [owise]

  rule #Ceil(Maybe(T, C, PARAMS)) => { true #Equals MaybeDef(T, C, PARAMS) } [simplification]

  // Constructor 0: Just
  syntax Value ::= Just(String, ConstantList) [function, injective]
  rule Just("ByteString", BS:ByteString) =>
    < delay
      (lam k_case_Just (lam k_case_Nothing [ k_case_Just k_arg_0 ]))
      k_arg_0 |-> < con bytestring BS >
    >

  rule Just("StakingCredential", [ CC:Int, PC:ConstantList ]) =>
    < delay
      (lam k_case_Just (lam k_case_Nothing [ k_case_Just k_arg_0 ]))
      k_arg_0 |-> StakingCredential(CC, PC)
    >

  syntax Bool ::= JustDef(String, ConstantList) [function, total]
  rule JustDef("ByteString", _:ByteString) => true
  rule JustDef("StakingCredential", [ CC:Int, PC:ConstantList ]) => StakingCredentialDef(CC, PC)
  rule JustDef(_, _) => false [owise]

  rule #Ceil(Just(T, PARAMS)) => { true #Equals JustDef(T, PARAMS) } [simplification]

  // Constructor 1: Nothing
  syntax Value ::= Nothing(ConstantList) [function, injective]
  rule Nothing(.ConstantList) =>
    < delay
      (lam k_case_Just (lam k_case_Nothing k_case_Nothing))
      .Map
    >

  syntax Bool ::= NothingDef(String, ConstantList) [function, total]
  rule NothingDef("ByteString", .ConstantList) => true
  rule NothingDef("StakingCredential", .ConstantList) => true
  rule NothingDef(_, _) => false [owise]

  rule #Ceil(Nothing(T, PARAMS)) => { true #Equals NothingDef(T, PARAMS) } [simplification]

  // Textual Data
  syntax TextualData ::= MaybeData(String, Int, ConstantList) [function, injective]
  rule MaybeData("ByteString", 0, BS:ByteString) => Constr 0 [ ByteString BS ]
  rule MaybeData("StakingCredential", 0, [ CC:Int, PC:ConstantList ]) => Constr 0 [ StakingCredentialData(CC, PC) ]

  rule MaybeData("ByteString", 1, .ConstantList) => Constr 1 [ .DataList ]
  rule MaybeData("StakingCredential", 1, .ConstantList) => Constr 1 [ .DataList ]

  rule #Ceil(MaybeData(T, C, PARAMS)) => { true #Equals MaybeDef(T, C, PARAMS) } [simplification]
```

## Global Lookup

### Auxiliaries

```k
  rule gLookup(k_fix) =>
    < delay (delay (lam k_f [ (lam k_s [ k_s k_s ]) (lam k_s (lam k_x [ [ k_f [ k_s k_s ] ] k_x ])) ])) .Map >
```

### Datatypes

#### Bool

```k
  rule gLookup(k_True)           => < delay (lam k_case_True (lam k_case_False k_case_True)) .Map >
  rule gLookup(k_False)          => < delay (lam k_case_True (lam k_case_False k_case_False)) .Map >
  rule gLookup(k_Bool_match)     => < lam k_x k_x .Map >

  syntax Term ::= "DECODER_CORE_BOOL" [alias]
  rule DECODER_CORE_BOOL =>
    [ (lam k_index [ (lam k_x [ [ [ [ (force (builtin ifThenElse)) [ [ (builtin equalsInteger) (force k_index) ] (con integer 1) ] ] (lam k_ds k_True ) ] (lam k_ds (force k_x) ) ] (con unit ()) ] ) (delay [ [ [ [ (force (builtin ifThenElse)) [ [ (builtin equalsInteger) (force k_index) ] (con integer 0) ] ] (lam k_ds k_False ) ] (lam k_ds (force k_error_PT1) ) ] (con unit ()) ] ) ] ) (delay [ (force (force (builtin fstPair)) ) [ (builtin unConstrData) k_d ] ] ) ]

  rule gLookup(k_fUnsafeFromDataBool_cunsafeFromBuiltinData) =>
    < lam k_d DECODER_CORE_BOOL .Map >
```

#### CustomTxInInfo

```k
  rule gLookup(k_CustomTxInInfo) =>
     < lam k_arg_0 (lam k_arg_1 (delay (lam k_case_CustomTxInInfo [ [ k_case_CustomTxInInfo k_arg_0 ] k_arg_1 ]))) .Map >

  rule gLookup(k_CustomTxInInfo_match) => < lam k_x k_x .Map >
```

#### CustomTxOut

```k
  rule gLookup(k_CustomTxOut) =>
    < lam k_arg_0 (lam k_arg_1 (lam k_arg_2 (delay (lam k_case_CustomTxOut [ [ [ k_case_CustomTxOut k_arg_0 ] k_arg_1 ] k_arg_2 ])))) .Map >

  rule gLookup(k_CustomTxOut_match) => < lam k_x k_x .Map >

  syntax Term ::= "DECODER_CORE_CUSTOMTXOUT" [alias]
  rule DECODER_CORE_CUSTOMTXOUT =>
    [ (lam k_args [ (lam k_addr_l [ [ [ k_CustomTxOut [ (force (builtin headList)) (force k_args) ] ] [ (force (builtin headList)) (force k_addr_l) ] ] [ (force (builtin headList)) [ (force (builtin tailList)) (force k_addr_l) ] ] ] ) (delay [ (force (builtin tailList)) (force k_args) ] ) ] ) (delay [ (force (force (builtin sndPair)) ) [ (builtin unConstrData) k_b ] ] ) ]

  rule gLookup(k_fUnsafeFromDataCustomTxOut_cunsafeFromBuiltinData) =>
    < lam k_b DECODER_CORE_CUSTOMTXOUT .Map >
```

#### DList

```k
  rule gLookup(k_Nil) =>
    < delay (delay (lam k_case_Nil (lam k_case_Cons k_case_Nil))) .Map >

  rule gLookup(k_Cons) =>
    < delay
      (lam k_arg_0 (lam k_arg_1 (delay (lam k_case_Nil (lam k_case_Cons [ [ k_case_Cons k_arg_0 ] k_arg_1 ])))))
      .Map
    >

  rule gLookup(k_Nil_match) => < delay (lam k_x k_x) .Map >
```

#### Maybe

```k
  rule gLookup(k_Just)    => < delay (lam k_arg (delay (lam k_case_Just (lam k_case_Nothing [ k_case_Just k_arg ] ) ) ) ) .Map >
  rule gLookup(k_Nothing) => < delay (delay (lam k_case_Just (lam k_case_Nothing k_case_Nothing))) .Map >

  rule gLookup(k_Maybe_match) => < delay (lam k_x k_x) .Map >
```

#### TxOutRef

```k
  rule gLookup(k_TxOutRef) =>
    < lam k_arg_0 (lam k_arg_1 (delay (lam case_TxOutRef [ case_TxOutRef k_arg_0 k_arg_1 ]))) .Map >

  rule gLookup(k_TxOutRef_match) =>
    < lam k_x k_x .Map >
```

### Functions

#### k_toScriptCredential

```k
rule gLookup(k_toScriptCredential) =>
  < lam k_b [ (lam k_cred (force [ [ (force [ k_Bool_match [ [ [ (force (builtin ifThenElse)) [ [ (builtin equalsInteger) [ (force (force (builtin fstPair)) ) (force k_cred) ] ] (con integer 1) ] ] k_True ] k_False ] ] ) (delay [ (force k_Just) [ (builtin unBData) [ (force (builtin headList)) [ (force (force (builtin sndPair)) ) (force k_cred) ] ] ] ] ) ] (delay (force k_Nothing) ) ] ) ) (delay [ (builtin unConstrData) [ (force (builtin headList)) [ (force (force (builtin sndPair)) ) [ (builtin unConstrData) k_b ] ] ] ] ) ] .Map >
```

```k
endmodule
```