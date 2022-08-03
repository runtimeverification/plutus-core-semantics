# UPLC Global Environment Instance

```k
require "uplc-genvironment.md"

module UPLC-GENVIRONMENT-INSTANCE
  imports SET
  imports UPLC-ID
  imports UPLC-MAP
  imports UPLC-SYNTAX
  imports UPLC-GENVIRONMENT

  // Error throwing with lambda
  syntax Term ::= "THROW_ERROR_LAM" [alias]
  rule THROW_ERROR_LAM =>
    (lam err_id
      (lam ds_0
        [
          (lam thunk_0 (error))
          [
            (force [ unit_match [ (force (builtin trace)) err_id unit_id ] ])
            unitval_id
          ]
        ]
      )
    )

  // Error throwing with delay
  syntax Term ::= "THROW_ERROR_DELAY" [alias]
  rule THROW_ERROR_DELAY =>
    (lam err_id
      (delay
        [
          (lam thunk_0 (error))
          [
            (force [ unit_match [ (force (builtin trace)) err_id unit_id ] ])
            unitval_id
          ]
        ]
      )
    )

  // Global environment
  rule #inKeysgEnv(X) =>
    X in
      SetItem(fix1_id:UplcId)
      SetItem(reconstructCaseError_id:UplcId)
      SetItem(tuple2_id:UplcId)
      SetItem(tuple2_match:UplcId)
      SetItem(unit_id:UplcId)
      SetItem(unit_match:UplcId)
      SetItem(unitval_id:UplcId)
      SetItem(pubKeyCredential_id:UplcId)
      SetItem(scriptCredential_id:UplcId)
      SetItem(credential_match:UplcId)
      SetItem(stakingHash_id:UplcId)
      SetItem(stakingPtr_id:UplcId)
      SetItem(stakingCredential_match:UplcId)
      SetItem(dCertDelegDeRegKey_id:UplcId)
      SetItem(dCertDelegDelegate_id:UplcId)
      SetItem(dCertDelegRegKey_id:UplcId)
      SetItem(dCertGenesis_id:UplcId)
      SetItem(dCertMir_id:UplcId)
      SetItem(dCertPoolRegister_id:UplcId)
      SetItem(dCertPoolRetire_id:UplcId)
      SetItem(dCert_match:UplcId)
      SetItem(true_id:UplcId)
      SetItem(false_id:UplcId)
      SetItem(bool_match:UplcId)
      SetItem(finite_id:UplcId)
      SetItem(negInf_id:UplcId)
      SetItem(posInf_id:UplcId)
      SetItem(extended_match:UplcId)
      SetItem(txOutRef_id:UplcId)
      SetItem(txOutRef_match:UplcId)
      SetItem(just_id:UplcId)
      SetItem(nothing_id:UplcId)
      SetItem(maybe_match:UplcId)
      SetItem(nil_id:UplcId)
      SetItem(cons_id:UplcId)
      SetItem(nil_match:UplcId)
      SetItem(address_id:UplcId)
      SetItem(address_match:UplcId)
      SetItem(txOut_id:UplcId)
      SetItem(txOut_match:UplcId)
      SetItem(certifying_id:UplcId)
      SetItem(minting_id:UplcId)
      SetItem(rewarding_id:UplcId)
      SetItem(spending_id:UplcId)
      SetItem(scriptPurpose_match:UplcId)
      SetItem(lowerBound_id:UplcId)
      SetItem(lowerBound_match:UplcId)
      SetItem(upperBound_id:UplcId)
      SetItem(upperBound_match:UplcId)
      SetItem(interval_id:UplcId)
      SetItem(interval_match:UplcId)
      SetItem(txInInfo_id:UplcId)
      SetItem(txInInfo_match:UplcId)
      SetItem(txInfo_id:UplcId)
      SetItem(txInfo_match:UplcId)
      SetItem(scriptContext_id:UplcId)
      SetItem(scriptContext_match:UplcId)

      SetItem(fUnsafeFromDatatuple2_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataCredential_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataDCert_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataBool_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataExtended_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataMaybe_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataMap_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataValue_id:UplcId)
      SetItem(fUnsafeFromDataNil_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataAddress_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataTxOut_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataLowerBound_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataUpperBound_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataInterval_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataTxInInfo_cunsafeFromBuiltinData:UplcId)
      SetItem(fUnsafeFromDataTxInfo_cunsafeFromBuiltinData:UplcId)

    [simplification]
```




## Global Lookup: Constructors and Matchers

```
  rule #gLookup(ID) => < delay (delay (lam f_0 [ (lam s_0 [ s_0 s_0 ]) (lam s_0 (lam x_0 [ [ f_0 [ s_0 s_0 ] ] x_0 ])) ])) .Map >
    requires ID ==K fix1_id:UplcId

  rule #gLookup(ID) => < con string "PT1" >
    requires ID ==K reconstructCaseError_id:UplcId

  rule #gLookup(ID) => < delay (delay (lam arg_0 (lam arg_1 (delay (lam case_Tuple2 [ case_Tuple2 arg_0 arg_1 ]))))) .Map >
    requires ID ==K tuple2_id:UplcId

  rule #gLookup(ID) => < delay (delay (lam x_0 x_0)) .Map >
    requires ID ==K tuple2_match_id:UplcId

  rule #gLookup(ID) => < delay (lam case_Unit case_Unit) .Map >
    requires ID ==K unit_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K unit_match:UplcId

  rule #gLookup(ID) => < con unit () >
    requires ID ==K unitval_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_PubKeyCredential (lam case_ScriptCredential [ case_PubKeyCredential arg_0 ]))) .Map >
    requires ID ==K pubKeyCredential_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_PubKeyCredential (lam case_ScriptCredential [ case_ScriptCredential arg_0 ]))) .Map >
    requires ID ==K scriptCredential_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K credential_match:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_StakingHash (lam case_StakingPtr [ case_StakingHash arg_0 ]))) .Map >
    requires ID ==K stakingHash_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (lam arg_1 (lam arg_2 (delay (lam case_StakingHash (lam case_StakingPtr [ case_StakingPtr arg_0 arg_1 arg_2 ]))))) .Map >
    requires ID ==K stakingPtr_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K stakingCredential_match:UplcId

  rule #gLookup(ID) =>
    < lam arg_0
      (delay
        (lam case_DCertDelegDeRegKey
          (lam case_DCertDelegDelegate
            (lam case_DCertDelegRegKey
              (lam case_DCertGenesis
                (lam case_DCertMir
                  (lam case_DCertPoolRegister
                    (lam case_DCertPoolRetire
                      [ case_DCertDelegDeRegKey arg_0 ]
                    )
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertDelegDeRegKey_id:UplcId

  rule #gLookup(ID) =>
    < lam arg_0
      (lam arg_1
        (delay
          (lam case_DCertDelegDeRegKey
            (lam case_DCertDelegDelegate
              (lam case_DCertDelegRegKey
                (lam case_DCertGenesis
                  (lam case_DCertMir
                    (lam case_DCertPoolRegister
                      (lam case_DCertPoolRetire
                        [ case_DCertDelegDelegate arg_0 arg_1 ]
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertDelegDelegate_id:UplcId

  rule #gLookup(ID) =>
    < lam arg_0
      (delay
        (lam case_DCertDelegDeRegKey
          (lam case_DCertDelegDelegate
            (lam case_DCertDelegRegKey
              (lam case_DCertGenesis
                (lam case_DCertMir
                  (lam case_DCertPoolRegister
                    (lam case_DCertPoolRetire
                      [ case_DCertDelegRegKey arg_0 ]
                    )
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertDelegRegKey_id:UplcId

  rule #gLookup(ID) =>
    < delay
      (lam case_DCertDelegDeRegKey
        (lam case_DCertDelegDelegate
          (lam case_DCertDelegRegKey
            (lam case_DCertGenesis
              (lam case_DCertMir
                (lam case_DCertPoolRegister
                  (lam case_DCertPoolRetire
                    case_DCertGenesis
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertGenesis_id:UplcId

  rule #gLookup(ID) =>
    < delay
      (lam case_DCertDelegDeRegKey
        (lam case_DCertDelegDelegate
          (lam case_DCertDelegRegKey
            (lam case_DCertGenesis
              (lam case_DCertMir
                (lam case_DCertPoolRegister
                  (lam case_DCertPoolRetire
                    case_DCertMir
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertMir_id:UplcId

  rule #gLookup(ID) =>
    < lam arg_0
      (lam arg_1
        (delay
          (lam case_DCertDelegDeRegKey
            (lam case_DCertDelegDelegate
              (lam case_DCertDelegRegKey
                (lam case_DCertGenesis
                  (lam case_DCertMir
                    (lam case_DCertPoolRegister
                      (lam case_DCertPoolRetire
                        [ case_DCertPoolRegister arg_0 arg_1 ]
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertPoolRegister_id:UplcId

  rule #gLookup(ID) =>
    < lam arg_0
      (lam arg_1
        (delay
          (lam case_DCertDelegDeRegKey
            (lam case_DCertDelegDelegate
              (lam case_DCertDelegRegKey
                (lam case_DCertGenesis
                  (lam case_DCertMir
                    (lam case_DCertPoolRegister
                      (lam case_DCertPoolRetire
                        [ case_DCertPoolRetire arg_0 arg_1 ]
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
      .Map
    >
    requires ID ==K dCertPoolRetire_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K dCert_match:UplcId

  rule #gLookup(ID) => < delay (lam case_True (lam case_False case_True)) .Map >
    requires ID ==K true_id:UplcId

  rule #gLookup(ID) => < delay (lam case_True (lam case_False case_False)) .Map >
    requires ID ==K false_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K bool_match:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_Finite (lam case_NegInf (lam case_PosInf [ case_Finite arg_0 ])))) .Map >
    requires ID ==K finite_id:UplcId

  rule #gLookup(ID) => < delay (delay (lam case_Finite (lam case_NegInf (lam case_PosInf case_NegInf)))) .Map >
    requires ID ==K negInf_id:UplcId

  rule #gLookup(ID) => < delay (delay (lam case_Finite (lam case_NegInf (lam case_PosInf case_PosInf)))) .Map >
    requires ID ==K posInf_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) .Map >
    requires ID ==K extended_match:UplcId

  rule #gLookup(ID) => < lam arg_0 (lam arg_1 (delay (lam case_TxOutRef [ case_TxOutRef arg_0 arg_1 ]))) .Map >
    requires ID ==K txOutRef_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K txOutRef_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (delay (lam case_Just (lam case_Nothing [ case_Just arg_0 ] )))) .Map >
    requires ID ==K just_id:UplcId

  rule #gLookup(ID) => < delay (delay (lam case_Just (lam case_Nothing case_Nothing))) .Map >
    requires ID ==K nothing_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) .Map >
    requires ID ==K maybe_match:UplcId

  rule #gLookup(ID) => < delay (delay (lam case_Nil (lam case_Cons case_Nil))) .Map >
    requires ID ==K nil_id:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_Nil (lam case_Cons [ case_Cons arg_0 arg_1 ]))))) .Map >
    requires ID ==K cons_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) .Map >
    requires ID ==K nil_match:UplcId

  rule #gLookup(ID) => < lam arg_0 (lam arg_1 (delay (lam case_Address [ case_Address arg_0 arg_1 ]))) .Map >
    requires ID ==K address_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K address_match:UplcId

  rule #gLookup(ID) => < lam arg_0 (lam arg_1 (lam arg_2 (delay (lam case_TxOut [ case_TxOut arg_0 arg_1 arg_2 ])))) .Map >
    requires ID ==K txOut_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K txOut_match:UplcId


  rule #gLookup(ID) => < lam arg_0 (delay (lam case_Certifying (lam case_Minting (lam case_Rewarding (lam case_Spending [ case_Certifying arg_0 ] ))))) .Map >
    requires ID ==K certifying_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_Certifying (lam case_Minting (lam case_Rewarding (lam case_Spending [ case_Minting arg_0 ] ))))) .Map >
    requires ID ==K minting_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_Certifying (lam case_Minting (lam case_Rewarding (lam case_Spending [ case_Rewarding arg_0 ] ))))) .Map >
    requires ID ==K rewarding_id:UplcId

  rule #gLookup(ID) => < lam arg_0 (delay (lam case_Certifying (lam case_Minting (lam case_Rewarding (lam case_Spending [ case_Spending arg_0 ] ))))) .Map >
    requires ID ==K spending_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K scriptPurpose_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_LowerBound [ case_LowerBound arg_0 arg_1 ])))) .Map >
    requires ID ==K lowerBound_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) >
    requires ID ==K lowerBound_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_UpperBound [ case_UpperBound arg_0 arg_1 ])))) .Map >
    requires ID ==K upperBound_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) >
    requires ID ==K upperBound_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_Interval [ case_Interval arg_0 arg_1 ])))) .Map >
    requires ID ==K interval_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) >
    requires ID ==K interval_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_TxInInfo [ case_TxInInfo arg_0 arg_1 ])))) .Map >
    requires ID ==K txInInfo_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) >
    requires ID ==K txInInfo_match:UplcId

  rule #gLookup(ID) =>
    < lam arg_0 (lam arg_1 (lam arg_2 (lam arg_3 (lam arg_4 (lam arg_5 (lam arg_6 (lam arg_7 (lam arg_8 (lam arg_9
        (delay (lam case_TxInfo [ case_TxInfo arg_0 arg_1 arg_2 arg_3 arg_4 arg_5 arg_6 arg_7 arg_8 arg_9 ])
      )))))))))
      .Map
    >
    requires ID ==K txInfo_id:UplcId

  rule #gLookup(ID) => < delay (lam x_0 x_0) >
    requires ID ==K txInfo_match:UplcId

  rule #gLookup(ID) => < delay (lam arg_0 (lam arg_1 (delay (lam case_ScriptContext [ case_ScriptContext arg_0 arg_1 ])))) .Map >
    requires ID ==K scriptContext_id:UplcId

  rule #gLookup(ID) => < lam x_0 x_0 .Map >
    requires ID ==K scriptContext_match:UplcId
```

## Global Lookup: Decoders

```k
  rule #gLookup(ID) =>
    < delay
      (delay
        (lam dUnsafeFromData_0
          (lam dUnsafeFromData_1
            (lam d_0
              [
                (lam tup_0
                  [
                    (lam t_0
                      [
                        [
                          (force (builtin ifThenElse))
                          [
                            (builtin equalsInteger)
                            [ (force (force (builtin fstPair))) (force tup_0) ]
                            (con integer 0)
                          ]
                          (lam ds_0
                            [
                              (force (force tuple2_id))
                              [
                                dUnsafeFromData_0
                                [ (force (builtin headList)) (force t_0) ]
                              ]
                              [
                                dUnsafeFromData_1
                                [
                                  (force (builtin headList))
                                  [ (force (builtin tailList)) (force t_0) ]
                                ]
                              ]
                            ]
                          )
                          [ THROW_ERROR_LAM reconstructCaseError_id ]
                        ]
                        unitval_id
                      ]
                    )
                    (delay [ (force (force (builtin sndPair))) (force tup_0) ])
                  ]
                )
                (delay [ (builtin unConstrData) d_0 ])
              ]
            )
          )
        )
      )
      .Map
    >
    requires ID ==K fUnsafeFromDatatuple2_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam index_0
              [
                [
                  [
                    [
                      (force (builtin ifThenElse))
                      [
                        (builtin equalsInteger)
                        (force index_0)
                        (con integer 1)
                      ]
                    ]
                    (lam ds_0
                      [
                        scriptCredential_id
                        [
                          (builtin unBData)
                          [
                            (force (builtin headList))
                            [ (force (force (builtin sndPair))) (force tup_0) ]
                          ]
                        ]
                      ]
                    )
                  ]
                  (lam ds_0
                    [
                      [
                        (force (builtin ifThenElse))
                        [
                          (builtin equalsInteger)
                          (force index_0)
                          (con integer 0)
                        ]
                        (lam ds_0
                          [
                            pubKeyCredential_id
                            [
                              (builtin unBData)
                              [
                                (force (builtin headList))
                                [ (force (force (builtin sndPair))) (force tup_0) ]
                              ]
                            ]
                          ]
                        )
                        [ THROW_ERROR_LAM reconstructCaseError_id ]
                      ]
                      unitval_id
                    ]
                  )
                ]
                unitval_id
              ]
            )
            (delay
              [
                (force (force (builtin fstPair)))
                (force tup_0)
              ]
            )
          ]
        )
        (delay
          [
            (builtin unConstrData) d_0
          ]
        )
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataCredential_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                (lam t_1
                  [
                    (lam index_0
                      [
                        [
                          (force (builtin ifThenElse))
                          [
                            (builtin equalsInteger)
                            (force index_0)
                            (con integer 1)
                          ]
                          (lam ds_0
                            [
                              stakingPtr_id
                              [
                                (builtin unIData)
                                [ (force (builtin headList)) (force t_0) ]
                              ]
                              [
                                (builtin unIData)
                                [ (force (builtin headList)) (force t_1) ]
                              ]
                              [
                                (builtin unIData)
                                [
                                  (force (builtin headList))
                                  [ (force (builtin tailList)) (force t_1) ]
                                ]
                              ]
                            ]
                          )
                          (lam ds_0
                            [
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    (force index_0)
                                    (con integer 0)
                                  ]
                                  (lam ds_0
                                    [
                                      stakingHash_id
                                      [
                                        fUnsafeFromDataCredential_cunsafeFromBuiltinData
                                        [
                                          (force (builtin headList))
                                          [ (force (force (builtin sndPair))) (force tup_0) ]
                                        ]
                                      ]
                                    ]
                                  )
                                ]
                                [ THROW_ERROR_LAM reconstructCaseError_id ]
                              ]
                              unitval_id
                            ]
                          )
                        ]
                        unitval_id
                      ]
                    )
                    (delay [ (force (force (builtin fstPair))) (force tup_0) ])
                  ]
                )
                (delay [ (force (builtin tailList)) (force t_0) ])
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                (lam t_1
                  [
                    (lam index_0
                      [
                        [
                          (force (builtin ifThenElse))
                          [
                            (builtin equalsInteger)
                            (force index_0)
                            (con integer 6)
                          ]
                          (lam ds_0 dCertMir_id)
                          (lam ds_0
                            [
                              [
                                (force (builtin ifThenElse))
                                [
                                  (builtin equalsInteger)
                                  (force index_0)
                                  (con integer 5)
                                ]
                                (lam ds_0 dCertGenesis_id)
                                (lam ds_0
                                  [
                                    [
                                      [
                                        (force (builtin ifThenElse))
                                        [
                                          (builtin equalsInteger)
                                          (force index_0)
                                          (con integer 4)
                                        ]
                                        (lam ds_0
                                          [
                                            dCertPoolRetire_id
                                            [
                                              (builtin unBData)
                                              [ (force (builtin headList)) (force t_0) ]
                                            ]
                                            [
                                              (builtin unIData)
                                              [ (force (builtin headList)) (force t_1) ]
                                            ]
                                          ]
                                        )
                                      ]
                                      (lam ds_0
                                        [
                                          [
                                            (force (builtin ifThenElse))
                                            [
                                              (builtin equalsInteger)
                                              (force index_0)
                                              (con integer 3)
                                            ]
                                            (lam ds_0
                                              [
                                                dCertPoolRegister_id
                                                [
                                                  (builtin unBData)
                                                  [ (force (builtin headList)) (force t_0) ]
                                                ]
                                                [
                                                  (builtin unBData)
                                                  [ (force (builtin headList)) (force t_1) ]
                                                ]
                                              ]
                                            )
                                            (lam ds_0
                                              [
                                                [
                                                  (force (builtin ifThenElse))
                                                  [
                                                    (builtin equalsInteger)
                                                    (force index_0)
                                                    (con integer 2)
                                                  ]
                                                  (lam ds_0
                                                    [
                                                      dCertDelegDelegate_id
                                                      [
                                                        fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                        [ (force (builtin headList)) (force t_0) ]
                                                      ]
                                                      [
                                                        (builtin unBData)
                                                        [ (force (builtin headList)) (force t_1) ]
                                                      ]
                                                    ]
                                                  )
                                                  (lam ds_0
                                                    [
                                                      [
                                                        (force (builtin ifThenElse))
                                                        [
                                                          (builtin equalsInteger)
                                                          (force index_0)
                                                          (con integer 1)
                                                        ]
                                                        (lam ds_0
                                                          [
                                                            dCertDelegDeRegKey_id
                                                            [
                                                              fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                              [ (force (builtin headList)) (force t_0) ]
                                                            ]
                                                          ]
                                                        )
                                                        (lam ds_0
                                                          [
                                                            [
                                                              (force (builtin ifThenElse))
                                                              [
                                                                (builtin equalsInteger)
                                                                (force index_0)
                                                                (con integer 0)
                                                              ]
                                                              (lam ds_0
                                                                [
                                                                  dCertDelegRegKey_id
                                                                  [
                                                                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                                    [ (force (builtin headList)) (force t_0) ]
                                                                  ]
                                                                ]
                                                              )
                                                              [ THROW_ERROR_LAM reconstructCaseError_id ]
                                                            ]
                                                            unitval_id
                                                          ]
                                                        )
                                                      ]
                                                      unitval_id
                                                    ]
                                                  )
                                                ]
                                                unitval_id
                                              ]
                                            )
                                          ]
                                          unitval_id
                                        ]
                                      )
                                    ]
                                    unitval_id
                                  ]
                                )
                              ]
                              unitval_id
                            ]
                          )
                        ]
                        unitval_id
                      ]
                    )
                    (delay [ (force (force (builtin fstPair))) (force tup_0) ])
                  ]
                )
                (delay [ (force (force (builtin sndPair))) (force t_0) ])
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataDCert_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam index_0
          [
            [
              (force (builtin ifThenElse))
              [
                (builtin equalsInteger)
                (force index_0)
                (con integer 1)
              ]
              (lam ds_0 true_id)
              (lam ds_0
                [
                  [
                    (force (builtin ifThenElse))
                    [
                      (builtin equalsInteger)
                      (force index_0)
                      (con integer 0)
                    ]
                    (lam ds_0 false_id)
                    [ THROW_ERROR_LAM reconstructCaseError_id ]
                  ]
                  unitval_id
                ]
              )
            ]
            unitval_id
          ]
        )
        (delay
          [
            (force (force (builtin fstPair)))
            [ (builtin unConstrData) d_0 ]
          ]
        )
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataBool_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < delay
      (lam dUnsafeFromData_0
        (lam d_0
          [
            (lam tup_0
              [
                (lam index_0
                  [
                    [
                      (force (builtin ifThenElse))
                      [
                        (builtin equalsInteger)
                        (force index_0)
                        (con integer 2)
                      ]
                      (lam ds_0 (force posInf_id))
                      (lam ds_0
                        [
                          [
                            (force (builtin ifThenElse))
                            [
                              (builtin equalsInteger)
                              (force index_0)
                              (con integer 1)
                            ]
                            (lam ds_0
                              [
                                (force finite_id)
                                [
                                  dUnsafeFromData_0
                                  [
                                    (force (builtin headList))
                                    [ (force (force (builtin sndPair))) (force tup_0) ]
                                  ]
                                ]
                              ]
                            )
                            (lam ds_0
                              [
                                [
                                  (force (builtin ifThenElse))
                                  [
                                    (builtin equalsInteger)
                                    (force index_0)
                                    (con integer 0)
                                  ]
                                  (lam ds_0 (force negInf_id))
                                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                                ]
                                unitval_id
                              ]
                            )
                          ]
                          unitval_id
                        ]
                      )
                    ]
                    unitval_id
                  ]
                )
                (delay
                  [
                    (force (force (builtin fstPair)))
                    (force tup_0)
                  ]
                )
              ]
            )
            (delay [ (builtin unConstrData) d_0 ])
          ]
        )
      )
      .Map
    >
    requires ID ==K fUnsafeFromDataExtended_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            [
              [
                (force (builtin ifThenElse))
                [
                  (builtin equalsInteger)
                  [ (force (force (builtin fstPair))) (force tup_0) ]
                  (con integer 0)
                ]
                (lam ds_0
                  [
                    (builtin unBData)
                    [
                      (force (builtin headList))
                      [
                        (force (force (builtin sndPair)))
                        (force tup_0)
                      ]
                    ]
                  ]
                )
              ]
              [ THROW_ERROR_LAM reconstructCaseError_id ]
            ]
            unitval_id
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
  requires ID ==K fUnsafeFromDataTxId_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  (force (builtin ifThenElse))
                  [
                    (builtin equalsInteger)
                    [ (force (force (builtin fstPair))) (force tup_0) ]
                    (con integer 0)
                  ]
                  (lam ds_0
                    [
                      txOutRef_id
                      [
                        fUnsafeFromDataTxId_cunsafeFromBuiltinData
                        [  (force (builtin headList)) (force t_0)]
                      ]
                      [
                        (builtin unIData)
                        [
                          (force (builtin headList))
                          [ (force (builtin tailList)) (force t_0) ]
                        ]
                      ]
                    ]
                  )
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay
              [
                (force (force (builtin sndPair)))
                (force tup_0)
              ]
            )
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < delay
      (lam dUnsafeFromData_0
        (lam d_0
          [
            (lam tup_0
              [
                (lam index_0
                  [
                    [
                      (force (builtin ifThenElse))
                      [
                        (builtin equalsInteger)
                        (force index_0)
                        (con integer 0)
                      ]
                      (lam ds_0
                        [
                          (force just_id)
                          [
                            dUnsafeFromData_0
                            [
                              (force (builtin headList))
                              [ (force (force (builtin sndPair))) (force tup_0) ]
                            ]
                          ]
                        ]
                      )
                      (lam ds_0
                        [
                          [
                            (force (builtin ifThenElse))
                            [
                              (builtin equalsInteger)
                              (force index_0)
                              (con integer 1)
                            ]
                            (lam ds_0 (force nothing_id))
                            [ THROW_ERROR_LAM reconstructCaseError_id ]
                          ]
                          unitval_id
                        ]
                      )
                    ]
                    unitval_id
                  ]
                )
                (delay [ (force (force (builtin fstPair))) (force tup_0) ])
              ]
            )
            (delay [ (builtin unConstrData) d_0 ])
          ]
        )
      )
      .Map
    >
    requires ID ==K fUnsafeFromDataMaybe_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < delay
      (delay
        (lam dUnsafeFromData_0
          (lam dUnsafeFromData_1
            [
              (lam go_0
                (lam d_0
                  [
                    go_0
                    [ (builtin unMapData) d_0 ]
                  ]
                )
              )
              [
                (lam arg_0 arg_0)
                [
                  (force (force fix1_id))
                  (lam go_0
                    (lam l_0
                      [
                        (lam tup_0
                          [
                            [
                              [ (force (force (builtin chooseList))) l_0 ]
                              (lam ds_0 (force nil_id))
                              (lam ds_0
                                [
                                  (force cons_id)
                                  [
                                    (force (force tuple2_id))
                                    [
                                      dUnsafeFromData_0
                                      [ (force (force (builtin fstPair))) (force tup_0) ]
                                    ]
                                    [
                                      dUnsafeFromData_1
                                      [ (force (force (builtin sndPair))) (force tup_0) ]
                                    ]
                                  ]
                                  [
                                    go_0
                                    [ (force (builtin tailList)) l_0 ]
                                  ]
                                ]
                              )
                            ]
                            unit_id
                          ]
                        )
                        (delay [ (force (builtin headList)) l_0 ] )
                      ]
                    )
                  )
                ]
              ]
            ]
          )
        )
      )
      .Map
    >
    requires ID ==K fUnsafeFromDataMap_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < delay
      [
        (force (force fUnsafeFromDataMap_cunsafeFromBuiltinData))
        (builtin unBData)
        (builtin unIData)
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataValue_id:UplcId

rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                (lam t_1
                  [
                    [
                      [
                        [
                          (force (builtin ifThenElse))
                          [
                            (builtin equalsInteger)
                            [ (force (force (builtin fstPair))) (force tup_0) ]
                            (con integer 0)
                          ]
                        ]
                        (lam ds_0
                          [
                            txOut_id
                            [
                              fUnsafeFromDataAddress_cunsafeFromBuiltinData
                              [ (force (builtin headList)) (force t_0) ]
                            ]
                            [
                              (force (force fUnsafeFromDataMap_cunsafeFromBuiltinData))
                              (builtin unBData)
                              (force fUnsafeFromDataValue_id)
                              [ (force (builtin headList)) (force t_1) ]
                            ]
                            [
                              (force fUnsafeFromDataMaybe_cunsafeFromBuiltinData)
                              (builtin unBData)
                              [
                                (force (builtin headList))
                                [ (force (builtin tailList)) (force t_1) ]
                              ]
                            ]
                          ]
                        )
                      ]
                      [ THROW_ERROR_LAM reconstructCaseError_id ]
                    ]
                    unitval_id
                  ]
                )
                (delay [ (force (builtin tailList)) (force t_0) ] )
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataNil_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  [
                    [
                      (force (builtin ifThenElse))
                      [
                        (builtin equalsInteger)
                        [ (force (force (builtin fstPair))) (force tup_0) ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_0
                      [
                        address_id
                        [
                          fUnsafeFromDataCredential_cunsafeFromBuiltinData
                          [ (force (builtin headList)) (force t_0) ]
                        ]
                        [
                          (force fUnsafeFromDataMaybe_cunsafeFromBuiltinData)
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                          [
                            (force (builtin headList))
                            [ (force (builtin tailList)) (force t_0) ]
                          ]
                        ]
                      ]
                    )
                  ]
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataAddress_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < delay
      (lam dUnsafeFromData_0
        [
          (lam go_0
            (lam d_0
              [
                go_0
                [ (builtin unListData) d_0 ]
              ]
            )
          )
          [
            (lam arg_0 arg_0)
            [
              (force (force fix1_id))
              (lam go_0
                (lam l_0
                  [
                    [
                      [ (force (force (builtin chooseList))) l_0 ]
                      (lam ds_0 (force nil_id))
                      (lam ds_0
                        [
                          (force cons_id)
                          [
                            dUnsafeFromData_0
                            [ (force (builtin headList)) l_0 ]
                          ]
                          [
                            go_0
                            [ (force (builtin tailList)) l_0 ]
                          ]
                        ]
                      )
                    ]
                    unit_id
                  ]
                )
              )
            ]
          ]
        ]
      )
      .Map
    >
    requires ID ==K fUnsafeFromDataTxOut_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam index_0
              [
                [
                  (force (builtin ifThenElse))
                  [
                    (builtin equalsInteger)
                    (force index_0)
                    (con integer 3)
                  ]
                  (lam ds_0
                    [
                      certifying_id
                      [
                        fUnsafeFromDataDCert_cunsafeFromBuiltinData
                        [
                          (force (builtin headList))
                          [ (force (force (builtin sndPair))) (force tup_0) ]
                        ]
                      ]
                    ]
                  )
                  (lam ds_0
                    [
                      [
                        (force (builtin ifThenElse))
                        [
                          (builtin equalsInteger)
                          (force index_0)
                          (con integer 2)
                        ]
                        (lam ds_0
                          [
                            rewarding_id
                            [
                              fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                              [
                                (force (builtin headList))
                                [ (force (force (builtin sndPair))) (force tup_0) ]
                              ]
                            ]
                          ]
                        )
                        (lam ds_0
                          [
                            [
                              (force (builtin ifThenElse))
                              [
                                (builtin equalsInteger)
                                (force index_0)
                                (con integer 1)
                              ]
                              (lam ds_0
                                [
                                  spending_id
                                  [
                                    fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData
                                    [
                                      (force (builtin headList))
                                      [ (force (force (builtin sndPair))) (force tup_0) ]
                                    ]
                                  ]
                                ]
                              )
                              (lam ds_0
                                [
                                  [
                                    (force (builtin ifThenElse))
                                    [
                                      (builtin equalsInteger)
                                      (force index_0)
                                      (con integer 0)
                                    ]
                                    (lam ds_0
                                      [
                                        minting_id
                                        [
                                          (builtin unBData)
                                          [
                                            (force (builtin headList))
                                            [ (force (force (builtin sndPair))) (force tup_0) ]
                                          ]
                                        ]
                                      ]
                                    )
                                    [ THROW_ERROR_LAM reconstructCaseError_id ]
                                  ]
                                  unitval_id
                                ]
                              )
                            ]
                            unitval_id
                          ]
                        )
                      ]
                      unitval_id
                    ]
                  )
                ]
                unitval_id
              ]
            )
            (delay [ (force (force (builtin fstPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataScriptPurpose_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  (force (builtin ifThenElse))
                  [
                    (builtin equalsInteger)
                    [ (force (force (builtin fstPair))) (force tup_0) ]
                    (con integer 0)
                  ]
                  (lam ds_0
                    [
                      [
                        (force lowerBound_id)
                        [
                          (force fUnsafeFromDataExtended_cunsafeFromBuiltinData)
                          (builtin unIData)
                          [ (force (builtin headList)) (force t_0) ]
                        ]
                      ]
                      [
                        fUnsafeFromDataBool_cunsafeFromBuiltinData
                        [
                          (force (builtin headList))
                          [ (force (builtin tailList)) (force t_0) ]
                        ]
                      ]
                    ]
                  )
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ] )
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataLowerBound_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  [
                    (force (builtin ifThenElse))
                    [
                      (builtin equalsInteger)
                      [ (force (force (builtin fstPair))) (force tup_0) ]
                      (con integer 0)
                    ]
                    (lam ds_0
                      [
                        (force upperBound_id)
                        [
                          (force fUnsafeFromDataExtended_cunsafeFromBuiltinData)
                          (builtin unIData)
                          [ (force (builtin headList)) (force t_0) ]
                        ]
                        [
                          fUnsafeFromDataBool_cunsafeFromBuiltinData
                          [
                            (force (builtin headList))
                            [ (force (builtin tailList)) (force t_0) ]
                          ]
                        ]
                      ]
                    )
                  ]
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataUpperBound_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  [
                    (force (builtin ifThenElse))
                    [
                      (builtin equalsInteger)
                      [
                        (force (force (builtin fstPair)))
                        (force tup_0)
                      ]
                      (con integer 0)
                    ]
                    (lam ds_0
                      [
                        (force interval_id)
                        [
                          fUnsafeFromDataLowerBound_cunsafeFromBuiltinData
                          [ (force (builtin headList)) (force t_0) ]
                        ]
                        [
                          fUnsafeFromDataUpperBound_cunsafeFromBuiltinData
                          [
                            (force (builtin headList))
                            [ (force (builtin tailList)) (force t_0) ]
                          ]
                        ]
                      ]
                    )
                  ]
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ]  )
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataInterval_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                [
                  (force (builtin ifThenElse))
                  [
                    (builtin equalsInteger)
                    [
                      (force (force (builtin fstPair)))
                      (force tup_0)
                    ]
                    (con integer 0)
                  ]
                  (lam ds_0
                    [
                      txInInfo_id
                      [
                        fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData
                        [ (force (builtin headList)) (force t_0) ]
                      ]
                      [
                        fUnsafeFromDataTxOut_cunsafeFromBuiltinData
                        [
                          (force (builtin headList))
                          [ (force (builtin tailList)) (force t_0) ]
                        ]
                      ]
                    ]
                  )
                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                ]
                unitval_id
              ]
            )
            (delay
              [
                (force (force (builtin sndPair)))
                (force tup_0)
              ]
            )
          ]
        )
        (delay [ (builtin unConstrData) d_0 ] )
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataTxInInfo_cunsafeFromBuiltinData:UplcId

  rule #gLookup(ID) =>
    < lam d_0
      [
        (lam tup_0
          [
            (lam t_0
              [
                (lam t_1
                  [
                    (lam t_2
                      [
                        (lam t_3
                          [
                            (lam t_4
                              [
                                (lam t_5
                                  [
                                    (lam t_6
                                      [
                                        (lam t_7
                                          [
                                            (lam t_8
                                              [
                                                [
                                                  (force (builtin ifThenElse))
                                                  [
                                                    (builtin equalsInteger)
                                                    [ (force (force (builtin fstPair))) (force tup_0) ]
                                                    (con integer 0)
                                                  ]
                                                  (lam ds_0
                                                    [
                                                      txInfo_id
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        fUnsafeFromDataTxInInfo_cunsafeFromBuiltinData
                                                        [ (force (builtin headList)) (force t_0) ]
                                                      ]
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        fUnsafeFromDataTxOut_cunsafeFromBuiltinData
                                                        [ (force (builtin headList)) (force t_1) ]
                                                      ]
                                                      // Param 3
                                                      [
                                                        (force (force fUnsafeFromDataMap_cunsafeFromBuiltinData))
                                                        (builtin unBData)
                                                        (force fUnsafeFromDataValue_id)
                                                        [ (force (builtin headList)) (force t_2) ]
                                                      ]
                                                      // Param 4
                                                      [
                                                        (force (force fUnsafeFromDataMap_cunsafeFromBuiltinData))
                                                        (builtin unBData)
                                                        (force fUnsafeFromDataValue_id)
                                                        [ (force (builtin headList)) (force t_3) ]
                                                      ]
                                                      // Param 5
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        fUnsafeFromDataDCert_cunsafeFromBuiltinData
                                                        [ (force (builtin headList)) (force t_4) ]
                                                      ]
                                                      // Param 6
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        [
                                                          (force (force fUnsafeFromDatatuple2_cunsafeFromBuiltinData))
                                                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData
                                                          (builtin unIData)
                                                        ]
                                                        [ (force (builtin headList)) (force t_5) ]
                                                      ]
                                                      // Param 7
                                                      [
                                                        fUnsafeFromDataInterval_cunsafeFromBuiltinData
                                                        [ (force (builtin headList)) (force t_6) ]
                                                      ]
                                                      // Param 8
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        (builtin unBData)
                                                        [ (force (builtin headList)) (force t_7) ]
                                                      ]
                                                      // Param 9
                                                      [
                                                        (force fUnsafeFromDataNil_cunsafeFromBuiltinData)
                                                        [
                                                          (force (force fUnsafeFromDatatuple2_cunsafeFromBuiltinData))
                                                          (builtin unBData)
                                                          (lam d_0 d_0)
                                                        ]
                                                        [ (force (builtin headList)) (force t_8) ]
                                                      ]
                                                      // Param 10
                                                      [
                                                        fUnsafeFromDataTxId_cunsafeFromBuiltinData
                                                        [
                                                          (force (builtin headList))
                                                          [ (force (builtin tailList)) (force t_8) ]
                                                        ]
                                                      ]
                                                    ]
                                                  )
                                                  [ THROW_ERROR_LAM reconstructCaseError_id ]
                                                ]
                                                unitval_id
                                              ]
                                            )
                                            (delay [ (force (builtin tailList)) (force t_7) ])
                                          ]
                                        )
                                        (delay [ (force (builtin tailList)) (force t_6) ])
                                      ]
                                    )
                                    (delay [ (force (builtin tailList)) (force t_5) ])
                                  ]
                                )
                                (delay [ (force (builtin tailList)) (force t_4) ])
                              ]
                            )
                            (delay [ (force (builtin tailList)) (force t_3) ])
                          ]
                        )
                        (delay [ (force (builtin tailList)) (force t_2) ])
                      ]
                    )
                    (delay [ (force (builtin tailList)) (force t_1) ])
                  ]
                )
                (delay [ (force (builtin tailList)) (force t_0) ])
              ]
            )
            (delay [ (force (force (builtin sndPair))) (force tup_0) ])
          ]
        )
        (delay [ (builtin unConstrData) d_0 ])
      ]
      .Map
    >
    requires ID ==K fUnsafeFromDataTxInfo_cunsafeFromBuiltinData:UplcId

endmodule
```