(program
  (let
    (nonrec)
    (termbind
      (strict)
      (vardecl
        bad_name_3439
        (all
          a_3440
          (type)
          (all b_3441 (type) (fun (fun a_3440 b_3441) (fun a_3440 b_3441)))
        )
      )
      (abs
        a_3435
        (type)
        (abs
          b_3436
          (type)
          (lam f_3437 (fun a_3435 b_3436) (lam a_3438 a_3435 [ f_3437 a_3438 ]))
        )
      )
    )
    (termbind
      (strict)
      (vardecl reconstructCaseError_2685 (con string))
      (con string "PT1")
    )
    (datatypebind
      (datatype
        (tyvardecl Unit_2563 (type))

        Unit_match_2565
        (vardecl Unit_2564 Unit_2563)
      )
    )
    (termbind
      (strict)
      (vardecl error_2559 (all a_2560 (type) (fun (con unit) a_2560)))
      (abs a_2557 (type) (lam thunk_2558 (con unit) (error a_2557)))
    )
    (termbind
      (strict)
      (vardecl
        trace_2555 (all a_2556 (type) (fun (con string) (fun a_2556 a_2556)))
      )
      (builtin trace)
    )
    (termbind (strict) (vardecl unitval_2554 (con unit)) (con unit ()))
    (termbind
      (nonstrict)
      (vardecl x_3430 Unit_2563)
      [
        { error_2559 Unit_2563 }
        [
          {
            [
              Unit_match_2565
              [
                [ { trace_2555 Unit_2563 } reconstructCaseError_2685 ] Unit_2564
              ]
            ]
            (con unit)
          }
          unitval_2554
        ]
      ]
    )
    (termbind
      (strict)
      (vardecl
        equalsInteger_2693 (fun (con integer) (fun (con integer) (con bool)))
      )
      (builtin equalsInteger)
    )
    (termbind
      (strict)
      (vardecl
        fst_2690
        (all
          a_2691
          (type)
          (all b_2692 (type) (fun [ [ (con pair) a_2691 ] b_2692 ] a_2691))
        )
      )
      (builtin fstPair)
    )
    (termbind
      (strict)
      (vardecl
        ifThenElse_2686
        (all a_2687 (type) (fun (con bool) (fun a_2687 (fun a_2687 a_2687))))
      )
      (builtin ifThenElse)
    )
    (termbind
      (strict)
      (vardecl
        unsafeDataAsConstr_2679
        (fun
          (con data) [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
        )
      )
      (builtin unConstrData)
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataUnit_cunsafeFromBuiltinData_3432
        (fun (con data) Unit_2563)
      )
      (lam
        d_3427
        (con data)
        [
          [
            [
              [
                { ifThenElse_2686 (fun (con unit) Unit_2563) }
                [
                  [
                    equalsInteger_2693
                    [
                      { { fst_2690 (con integer) } [ (con list) (con data) ] }
                      [ unsafeDataAsConstr_2679 d_3427 ]
                    ]
                  ]
                  (con integer 0)
                ]
              ]
              (lam ds_3428 (con unit) Unit_2564)
            ]
            (lam ds_3431 (con unit) x_3430)
          ]
          unitval_2554
        ]
      )
    )
    (termbind
      (nonstrict)
      (vardecl
        fUnsafeFromDataUnit_3433
        [ (lam a_3434 (type) (fun (con data) a_3434)) Unit_2563 ]
      )
      fUnsafeFromDataUnit_cunsafeFromBuiltinData_3432
    )
    (datatypebind
      (datatype
        (tyvardecl AdditiveMonoid_2541 (fun (type) (type)))
        (tyvardecl a_2544 (type))
        AdditiveMonoid_match_2543
        (vardecl
          CConsAdditiveMonoid_2542
          (fun
            [ (lam a_2545 (type) (fun a_2545 (fun a_2545 a_2545))) a_2544 ]
            (fun a_2544 [ AdditiveMonoid_2541 a_2544 ])
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool_2527 (type))

        Bool_match_2530
        (vardecl True_2528 Bool_2527) (vardecl False_2529 Bool_2527)
      )
    )
    (termbind
      (strict)
      (vardecl bad_name_2538 (fun Bool_2527 (fun Bool_2527 Bool_2527)))
      (lam
        l_2531
        Bool_2527
        (lam
          r_2532
          Bool_2527
          {
            [
              [
                { [ Bool_match_2530 l_2531 ] (all dead_2534 (type) Bool_2527) }
                (abs dead_2535 (type) True_2528)
              ]
              (abs dead_2536 (type) r_2532)
            ]
            (all dead_2537 (type) dead_2537)
          }
        )
      )
    )
    (termbind
      (nonstrict)
      (vardecl fAdditiveMonoidBool_3263 [ AdditiveMonoid_2541 Bool_2527 ])
      [ [ { CConsAdditiveMonoid_2542 Bool_2527 } bad_name_2538 ] False_2529 ]
    )
    (termbind
      (strict)
      (vardecl
        p1AdditiveMonoid_3189
        (all
          a_3190
          (type)
          (fun
            [ AdditiveMonoid_2541 a_3190 ]
            [ (lam a_3191 (type) (fun a_3191 (fun a_3191 a_3191))) a_3190 ]
          )
        )
      )
      (abs
        a_3182
        (type)
        (lam
          v_3183
          [ AdditiveMonoid_2541 a_3182 ]
          [
            {
              [ { AdditiveMonoid_match_2543 a_3182 } v_3183 ]
              [ (lam a_3185 (type) (fun a_3185 (fun a_3185 a_3185))) a_3182 ]
            }
            (lam
              v_3186
              [ (lam a_3187 (type) (fun a_3187 (fun a_3187 a_3187))) a_3182 ]
              (lam v_3188 a_3182 v_3186)
            )
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Monoid_3177 (fun (type) (type)))
        (tyvardecl a_3180 (type))
        Monoid_match_3179
        (vardecl
          CConsMonoid_3178
          (fun
            [ (lam a_3181 (type) (fun a_3181 (fun a_3181 a_3181))) a_3180 ]
            (fun a_3180 [ Monoid_3177 a_3180 ])
          )
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        zero_2552
        (all a_2553 (type) (fun [ AdditiveMonoid_2541 a_2553 ] a_2553))
      )
      (abs
        a_2546
        (type)
        (lam
          v_2547
          [ AdditiveMonoid_2541 a_2546 ]
          [
            { [ { AdditiveMonoid_match_2543 a_2546 } v_2547 ] a_2546 }
            (lam
              v_2549
              [ (lam a_2550 (type) (fun a_2550 (fun a_2550 a_2550))) a_2546 ]
              (lam v_2551 a_2546 v_2551)
            )
          ]
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fMonoidSum_3199
        (all
          a_3200
          (type)
          (fun
            [ AdditiveMonoid_2541 a_3200 ]
            [ Monoid_3177 [ (lam a_3201 (type) a_3201) a_3200 ] ]
          )
        )
      )
      (abs
        a_3192
        (type)
        (lam
          v_3193
          [ AdditiveMonoid_2541 a_3192 ]
          [
            [
              { CConsMonoid_3178 [ (lam a_3194 (type) a_3194) a_3192 ] }
              (lam
                eta_3195
                [ (lam a_3196 (type) a_3196) a_3192 ]
                (lam
                  eta_3197
                  [ (lam a_3198 (type) a_3198) a_3192 ]
                  [
                    [ [ { p1AdditiveMonoid_3189 a_3192 } v_3193 ] eta_3195 ]
                    eta_3197
                  ]
                )
              )
            ]
            [ { zero_2552 a_3192 } v_3193 ]
          ]
        )
      )
    )
    (termbind
      (nonstrict)
      (vardecl
        dMonoid_3273 [ Monoid_3177 [ (lam a_3274 (type) a_3274) Bool_2527 ] ]
      )
      [ { fMonoidSum_3199 Bool_2527 } fAdditiveMonoidBool_3263 ]
    )
    (termbind
      (strict)
      (vardecl
        equalsByteString_3169
        (fun (con bytestring) (fun (con bytestring) (con bool)))
      )
      (builtin equalsByteString)
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef_2624 (type))

        TxOutRef_match_2626
        (vardecl
          TxOutRef_2625 (fun (con bytestring) (fun (con integer) TxOutRef_2624))
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fEqTxOutRef_c_3262 (fun TxOutRef_2624 (fun TxOutRef_2624 Bool_2527))
      )
      (lam
        l_3237
        TxOutRef_2624
        (lam
          r_3238
          TxOutRef_2624
          {
            [
              [
                {
                  [
                    Bool_match_2530
                    [
                      [
                        [
                          { ifThenElse_2686 Bool_2527 }
                          [
                            [
                              equalsByteString_3169
                              [
                                {
                                  [ TxOutRef_match_2626 l_3237 ]
                                  (con bytestring)
                                }
                                (lam
                                  ds_3247
                                  (con bytestring)
                                  (lam ds_3248 (con integer) ds_3247)
                                )
                              ]
                            ]
                            [
                              {
                                [ TxOutRef_match_2626 r_3238 ] (con bytestring)
                              }
                              (lam
                                ds_3250
                                (con bytestring)
                                (lam ds_3251 (con integer) ds_3250)
                              )
                            ]
                          ]
                        ]
                        True_2528
                      ]
                      False_2529
                    ]
                  ]
                  (all dead_3252 (type) Bool_2527)
                }
                (abs
                  dead_3253
                  (type)
                  [
                    [
                      [
                        { ifThenElse_2686 Bool_2527 }
                        [
                          [
                            equalsInteger_2693
                            [
                              { [ TxOutRef_match_2626 l_3237 ] (con integer) }
                              (lam
                                ds_3255
                                (con bytestring)
                                (lam ds_3256 (con integer) ds_3256)
                              )
                            ]
                          ]
                          [
                            { [ TxOutRef_match_2626 r_3238 ] (con integer) }
                            (lam
                              ds_3258
                              (con bytestring)
                              (lam ds_3259 (con integer) ds_3259)
                            )
                          ]
                        ]
                      ]
                      True_2528
                    ]
                    False_2529
                  ]
                )
              ]
              (abs dead_3260 (type) False_2529)
            ]
            (all dead_3261 (type) dead_3261)
          }
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        p1Monoid_3217
        (all
          a_3218
          (type)
          (fun
            [ Monoid_3177 a_3218 ]
            [ (lam a_3219 (type) (fun a_3219 (fun a_3219 a_3219))) a_3218 ]
          )
        )
      )
      (abs
        a_3210
        (type)
        (lam
          v_3211
          [ Monoid_3177 a_3210 ]
          [
            {
              [ { Monoid_match_3179 a_3210 } v_3211 ]
              [ (lam a_3213 (type) (fun a_3213 (fun a_3213 a_3213))) a_3210 ]
            }
            (lam
              v_3214
              [ (lam a_3215 (type) (fun a_3215 (fun a_3215 a_3215))) a_3210 ]
              (lam v_3216 a_3210 v_3214)
            )
          ]
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        mempty_3208 (all a_3209 (type) (fun [ Monoid_3177 a_3209 ] a_3209))
      )
      (abs
        a_3202
        (type)
        (lam
          v_3203
          [ Monoid_3177 a_3202 ]
          [
            { [ { Monoid_match_3179 a_3202 } v_3203 ] a_3202 }
            (lam
              v_3205
              [ (lam a_3206 (type) (fun a_3206 (fun a_3206 a_3206))) a_3202 ]
              (lam v_3207 a_3202 v_3207)
            )
          ]
        )
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List_2575 (fun (type) (type)))
          (tyvardecl a_2579 (type))
          Nil_match_2578
          (vardecl Nil_2576 [ List_2575 a_2579 ])
          (vardecl
            Cons_2577
            (fun a_2579 (fun [ List_2575 a_2579 ] [ List_2575 a_2579 ]))
          )
        )
      )
      (let
        (rec)
        (termbind
          (strict)
          (vardecl
            fFoldableNil_cfoldMap_3220
            (all
              m_3221
              (type)
              (all
                a_3222
                (type)
                (fun
                  [ Monoid_3177 m_3221 ]
                  (fun (fun a_3222 m_3221) (fun [ List_2575 a_3222 ] m_3221))
                )
              )
            )
          )
          (abs
            m_3223
            (type)
            (abs
              a_3224
              (type)
              (lam
                dMonoid_3225
                [ Monoid_3177 m_3223 ]
                (let
                  (nonrec)
                  (termbind
                    (nonstrict)
                    (vardecl
                      dSemigroup_3226
                      [
                        (lam a_3227 (type) (fun a_3227 (fun a_3227 a_3227)))
                        m_3223
                      ]
                    )
                    [ { p1Monoid_3217 m_3223 } dMonoid_3225 ]
                  )
                  (lam
                    ds_3228
                    (fun a_3224 m_3223)
                    (lam
                      ds_3229
                      [ List_2575 a_3224 ]
                      {
                        [
                          [
                            {
                              [ { Nil_match_2578 a_3224 } ds_3229 ]
                              (all dead_3231 (type) m_3223)
                            }
                            (abs
                              dead_3232
                              (type)
                              [ { mempty_3208 m_3223 } dMonoid_3225 ]
                            )
                          ]
                          (lam
                            x_3233
                            a_3224
                            (lam
                              xs_3234
                              [ List_2575 a_3224 ]
                              (abs
                                dead_3235
                                (type)
                                [
                                  [ dSemigroup_3226 [ ds_3228 x_3233 ] ]
                                  [
                                    [
                                      [
                                        {
                                          { fFoldableNil_cfoldMap_3220 m_3223 }
                                          a_3224
                                        }
                                        dMonoid_3225
                                      ]
                                      ds_3228
                                    ]
                                    xs_3234
                                  ]
                                ]
                              )
                            )
                          )
                        ]
                        (all dead_3236 (type) dead_3236)
                      }
                    )
                  )
                )
              )
            )
          )
        )
        (let
          (nonrec)
          (termbind
            (strict)
            (vardecl
              equalsByteString_3172
              (fun (con bytestring) (fun (con bytestring) Bool_2527))
            )
            (lam
              x_3170
              (con bytestring)
              (lam
                y_3171
                (con bytestring)
                [
                  [
                    [
                      { ifThenElse_2686 Bool_2527 }
                      [ [ equalsByteString_3169 x_3170 ] y_3171 ]
                    ]
                    True_2528
                  ]
                  False_2529
                ]
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl
                Tuple3_3103 (fun (type) (fun (type) (fun (type) (type))))
              )
              (tyvardecl a_3106 (type))
              (tyvardecl b_3107 (type))
              (tyvardecl c_3108 (type))
              Tuple3_match_3105
              (vardecl
                Tuple3_3104
                (fun
                  a_3106
                  (fun
                    b_3107
                    (fun c_3108 [ [ [ Tuple3_3103 a_3106 ] b_3107 ] c_3108 ])
                  )
                )
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Tuple2_2582 (fun (type) (fun (type) (type))))
              (tyvardecl a_2585 (type)) (tyvardecl b_2586 (type))
              Tuple2_match_2584
              (vardecl
                Tuple2_2583
                (fun a_2585 (fun b_2586 [ [ Tuple2_2582 a_2585 ] b_2586 ]))
              )
            )
          )
          (let
            (rec)
            (termbind
              (strict)
              (vardecl
                goInner_3144
                (fun
                  (con bytestring)
                  (fun
                    [
                      List_2575
                      [
                        [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        List_2575
                        [ [ Tuple2_2582 (con bytestring) ] (con integer) ]
                      ]
                      [
                        List_2575
                        [
                          [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                )
              )
              (lam
                ds_3145
                (con bytestring)
                (lam
                  acc_3146
                  [
                    List_2575
                    [
                      [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                      (con integer)
                    ]
                  ]
                  (lam
                    ds_3147
                    [
                      List_2575
                      [ [ Tuple2_2582 (con bytestring) ] (con integer) ]
                    ]
                    {
                      [
                        [
                          {
                            [
                              {
                                Nil_match_2578
                                [
                                  [ Tuple2_2582 (con bytestring) ] (con integer)
                                ]
                              }
                              ds_3147
                            ]
                            (all
                              dead_3149
                              (type)
                              [
                                List_2575
                                [
                                  [
                                    [ Tuple3_3103 (con bytestring) ]
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                            )
                          }
                          (abs dead_3150 (type) acc_3146)
                        ]
                        (lam
                          ipv_3151
                          [ [ Tuple2_2582 (con bytestring) ] (con integer) ]
                          (lam
                            ipv_3152
                            [
                              List_2575
                              [ [ Tuple2_2582 (con bytestring) ] (con integer) ]
                            ]
                            (abs
                              dead_3153
                              (type)
                              [
                                {
                                  [
                                    {
                                      { Tuple2_match_2584 (con bytestring) }
                                      (con integer)
                                    }
                                    ipv_3151
                                  ]
                                  [
                                    List_2575
                                    [
                                      [
                                        [ Tuple3_3103 (con bytestring) ]
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                  ]
                                }
                                (lam
                                  tn_3155
                                  (con bytestring)
                                  (lam
                                    a_3156
                                    (con integer)
                                    {
                                      [
                                        [
                                          {
                                            [
                                              Bool_match_2530
                                              [
                                                [
                                                  [
                                                    {
                                                      ifThenElse_2686 Bool_2527
                                                    }
                                                    [
                                                      [
                                                        equalsInteger_2693
                                                        a_3156
                                                      ]
                                                      (con integer 0)
                                                    ]
                                                  ]
                                                  True_2528
                                                ]
                                                False_2529
                                              ]
                                            ]
                                            (all
                                              dead_3158
                                              (type)
                                              [
                                                List_2575
                                                [
                                                  [
                                                    [
                                                      Tuple3_3103
                                                      (con bytestring)
                                                    ]
                                                    (con bytestring)
                                                  ]
                                                  (con integer)
                                                ]
                                              ]
                                            )
                                          }
                                          (abs
                                            dead_3159
                                            (type)
                                            [
                                              [
                                                [ goInner_3144 ds_3145 ]
                                                acc_3146
                                              ]
                                              ipv_3152
                                            ]
                                          )
                                        ]
                                        (abs
                                          dead_3160
                                          (type)
                                          [
                                            [
                                              [ goInner_3144 ds_3145 ]
                                              [
                                                [
                                                  {
                                                    Cons_2577
                                                    [
                                                      [
                                                        [
                                                          Tuple3_3103
                                                          (con bytestring)
                                                        ]
                                                        (con bytestring)
                                                      ]
                                                      (con integer)
                                                    ]
                                                  }
                                                  [
                                                    [
                                                      [
                                                        {
                                                          {
                                                            {
                                                              Tuple3_3104
                                                              (con bytestring)
                                                            }
                                                            (con bytestring)
                                                          }
                                                          (con integer)
                                                        }
                                                        ds_3145
                                                      ]
                                                      tn_3155
                                                    ]
                                                    a_3156
                                                  ]
                                                ]
                                                acc_3146
                                              ]
                                            ]
                                            ipv_3152
                                          ]
                                        )
                                      ]
                                      (all dead_3161 (type) dead_3161)
                                    }
                                  )
                                )
                              ]
                            )
                          )
                        )
                      ]
                      (all dead_3162 (type) dead_3162)
                    }
                  )
                )
              )
            )
            (let
              (rec)
              (termbind
                (strict)
                (vardecl
                  goOuter_3114
                  (fun
                    [
                      List_2575
                      [
                        [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        List_2575
                        [
                          [ Tuple2_2582 (con bytestring) ]
                          [
                            [
                              (lam
                                k_3115
                                (type)
                                (lam
                                  v_3116
                                  (type)
                                  [
                                    List_2575 [ [ Tuple2_2582 k_3115 ] v_3116 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            (con integer)
                          ]
                        ]
                      ]
                      [
                        List_2575
                        [
                          [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                )
                (lam
                  acc_3117
                  [
                    List_2575
                    [
                      [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                      (con integer)
                    ]
                  ]
                  (lam
                    ds_3118
                    [
                      List_2575
                      [
                        [ Tuple2_2582 (con bytestring) ]
                        [
                          [
                            (lam
                              k_3119
                              (type)
                              (lam
                                v_3120
                                (type)
                                [ List_2575 [ [ Tuple2_2582 k_3119 ] v_3120 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                    ]
                    {
                      [
                        [
                          {
                            [
                              {
                                Nil_match_2578
                                [
                                  [ Tuple2_2582 (con bytestring) ]
                                  [
                                    [
                                      (lam
                                        k_3124
                                        (type)
                                        (lam
                                          v_3125
                                          (type)
                                          [
                                            List_2575
                                            [ [ Tuple2_2582 k_3124 ] v_3125 ]
                                          ]
                                        )
                                      )
                                      (con bytestring)
                                    ]
                                    (con integer)
                                  ]
                                ]
                              }
                              ds_3118
                            ]
                            (all
                              dead_3126
                              (type)
                              [
                                List_2575
                                [
                                  [
                                    [ Tuple3_3103 (con bytestring) ]
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                            )
                          }
                          (abs dead_3127 (type) acc_3117)
                        ]
                        (lam
                          ipv_3128
                          [
                            [ Tuple2_2582 (con bytestring) ]
                            [
                              [
                                (lam
                                  k_3129
                                  (type)
                                  (lam
                                    v_3130
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_3129 ] v_3130 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                          (lam
                            ipv_3131
                            [
                              List_2575
                              [
                                [ Tuple2_2582 (con bytestring) ]
                                [
                                  [
                                    (lam
                                      k_3132
                                      (type)
                                      (lam
                                        v_3133
                                        (type)
                                        [
                                          List_2575
                                          [ [ Tuple2_2582 k_3132 ] v_3133 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                            ]
                            (abs
                              dead_3134
                              (type)
                              [
                                {
                                  [
                                    {
                                      { Tuple2_match_2584 (con bytestring) }
                                      [
                                        [
                                          (lam
                                            k_3138
                                            (type)
                                            (lam
                                              v_3139
                                              (type)
                                              [
                                                List_2575
                                                [
                                                  [ Tuple2_2582 k_3138 ] v_3139
                                                ]
                                              ]
                                            )
                                          )
                                          (con bytestring)
                                        ]
                                        (con integer)
                                      ]
                                    }
                                    ipv_3128
                                  ]
                                  [
                                    List_2575
                                    [
                                      [
                                        [ Tuple3_3103 (con bytestring) ]
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                  ]
                                }
                                (lam
                                  cs_3140
                                  (con bytestring)
                                  (lam
                                    m_3141
                                    [
                                      [
                                        (lam
                                          k_3142
                                          (type)
                                          (lam
                                            v_3143
                                            (type)
                                            [
                                              List_2575
                                              [ [ Tuple2_2582 k_3142 ] v_3143 ]
                                            ]
                                          )
                                        )
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                    [
                                      [
                                        goOuter_3114
                                        [
                                          [ [ goInner_3144 cs_3140 ] acc_3117 ]
                                          m_3141
                                        ]
                                      ]
                                      ipv_3131
                                    ]
                                  )
                                )
                              ]
                            )
                          )
                        )
                      ]
                      (all dead_3163 (type) dead_3163)
                    }
                  )
                )
              )
              (let
                (nonrec)
                (termbind
                  (strict)
                  (vardecl
                    flattenValue_3164
                    (fun
                      [
                        [
                          (lam
                            k_3165
                            (type)
                            (lam
                              v_3166
                              (type)
                              [ List_2575 [ [ Tuple2_2582 k_3165 ] v_3166 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        [
                          [
                            (lam
                              k_3167
                              (type)
                              (lam
                                v_3168
                                (type)
                                [ List_2575 [ [ Tuple2_2582 k_3167 ] v_3168 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                      [
                        List_2575
                        [
                          [ [ Tuple3_3103 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                  (lam
                    v_3109
                    [
                      [
                        (lam
                          k_3110
                          (type)
                          (lam
                            v_3111
                            (type)
                            [ List_2575 [ [ Tuple2_2582 k_3110 ] v_3111 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      [
                        [
                          (lam
                            k_3112
                            (type)
                            (lam
                              v_3113
                              (type)
                              [ List_2575 [ [ Tuple2_2582 k_3112 ] v_3113 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        (con integer)
                      ]
                    ]
                    [
                      [
                        goOuter_3114
                        {
                          Nil_2576
                          [
                            [
                              [ Tuple3_3103 (con bytestring) ] (con bytestring)
                            ]
                            (con integer)
                          ]
                        }
                      ]
                      v_3109
                    ]
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Credential_2605 (type))

                    Credential_match_2608
                    (vardecl
                      PubKeyCredential_2606
                      (fun (con bytestring) Credential_2605)
                    )
                    (vardecl
                      ScriptCredential_2607
                      (fun (con bytestring) Credential_2605)
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl StakingCredential_2609 (type))

                    StakingCredential_match_2612
                    (vardecl
                      StakingHash_2610
                      (fun Credential_2605 StakingCredential_2609)
                    )
                    (vardecl
                      StakingPtr_2611
                      (fun
                        (con integer)
                        (fun
                          (con integer)
                          (fun (con integer) StakingCredential_2609)
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl DCert_2650 (type))

                    DCert_match_2658
                    (vardecl
                      DCertDelegDeRegKey_2651
                      (fun StakingCredential_2609 DCert_2650)
                    )
                    (vardecl
                      DCertDelegDelegate_2652
                      (fun
                        StakingCredential_2609 (fun (con bytestring) DCert_2650)
                      )
                    )
                    (vardecl
                      DCertDelegRegKey_2653
                      (fun StakingCredential_2609 DCert_2650)
                    )
                    (vardecl DCertGenesis_2654 DCert_2650)
                    (vardecl DCertMir_2655 DCert_2650)
                    (vardecl
                      DCertPoolRegister_2656
                      (fun (con bytestring) (fun (con bytestring) DCert_2650))
                    )
                    (vardecl
                      DCertPoolRetire_2657
                      (fun (con bytestring) (fun (con integer) DCert_2650))
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl ScriptPurpose_2670 (type))

                    ScriptPurpose_match_2675
                    (vardecl
                      Certifying_2671 (fun DCert_2650 ScriptPurpose_2670)
                    )
                    (vardecl
                      Minting_2672 (fun (con bytestring) ScriptPurpose_2670)
                    )
                    (vardecl
                      Rewarding_2673
                      (fun StakingCredential_2609 ScriptPurpose_2670)
                    )
                    (vardecl
                      Spending_2674 (fun TxOutRef_2624 ScriptPurpose_2670)
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Extended_2631 (fun (type) (type)))
                    (tyvardecl a_2636 (type))
                    Extended_match_2635
                    (vardecl Finite_2632 (fun a_2636 [ Extended_2631 a_2636 ]))
                    (vardecl NegInf_2633 [ Extended_2631 a_2636 ])
                    (vardecl PosInf_2634 [ Extended_2631 a_2636 ])
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl LowerBound_2641 (fun (type) (type)))
                    (tyvardecl a_2644 (type))
                    LowerBound_match_2643
                    (vardecl
                      LowerBound_2642
                      (fun
                        [ Extended_2631 a_2644 ]
                        (fun Bool_2527 [ LowerBound_2641 a_2644 ])
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl UpperBound_2637 (fun (type) (type)))
                    (tyvardecl a_2640 (type))
                    UpperBound_match_2639
                    (vardecl
                      UpperBound_2638
                      (fun
                        [ Extended_2631 a_2640 ]
                        (fun Bool_2527 [ UpperBound_2637 a_2640 ])
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Interval_2645 (fun (type) (type)))
                    (tyvardecl a_2648 (type))
                    Interval_match_2647
                    (vardecl
                      Interval_2646
                      (fun
                        [ LowerBound_2641 a_2648 ]
                        (fun
                          [ UpperBound_2637 a_2648 ] [ Interval_2645 a_2648 ]
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Maybe_2597 (fun (type) (type)))
                    (tyvardecl a_2601 (type))
                    Maybe_match_2600
                    (vardecl Just_2598 (fun a_2601 [ Maybe_2597 a_2601 ]))
                    (vardecl Nothing_2599 [ Maybe_2597 a_2601 ])
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Address_2613 (type))

                    Address_match_2615
                    (vardecl
                      Address_2614
                      (fun
                        Credential_2605
                        (fun [ Maybe_2597 StakingCredential_2609 ] Address_2613)
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxOut_2616 (type))

                    TxOut_match_2618
                    (vardecl
                      TxOut_2617
                      (fun
                        Address_2613
                        (fun
                          [
                            [
                              (lam
                                k_2619
                                (type)
                                (lam
                                  v_2620
                                  (type)
                                  [
                                    List_2575 [ [ Tuple2_2582 k_2619 ] v_2620 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_2621
                                  (type)
                                  (lam
                                    v_2622
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_2621 ] v_2622 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                          (fun [ Maybe_2597 (con bytestring) ] TxOut_2616)
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxInInfo_2627 (type))

                    TxInInfo_match_2629
                    (vardecl
                      TxInInfo_2628
                      (fun TxOutRef_2624 (fun TxOut_2616 TxInInfo_2627))
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxInfo_2659 (type))

                    TxInfo_match_2661
                    (vardecl
                      TxInfo_2660
                      (fun
                        [ List_2575 TxInInfo_2627 ]
                        (fun
                          [ List_2575 TxOut_2616 ]
                          (fun
                            [
                              [
                                (lam
                                  k_2662
                                  (type)
                                  (lam
                                    v_2663
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_2662 ] v_2663 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              [
                                [
                                  (lam
                                    k_2664
                                    (type)
                                    (lam
                                      v_2665
                                      (type)
                                      [
                                        List_2575
                                        [ [ Tuple2_2582 k_2664 ] v_2665 ]
                                      ]
                                    )
                                  )
                                  (con bytestring)
                                ]
                                (con integer)
                              ]
                            ]
                            (fun
                              [
                                [
                                  (lam
                                    k_2666
                                    (type)
                                    (lam
                                      v_2667
                                      (type)
                                      [
                                        List_2575
                                        [ [ Tuple2_2582 k_2666 ] v_2667 ]
                                      ]
                                    )
                                  )
                                  (con bytestring)
                                ]
                                [
                                  [
                                    (lam
                                      k_2668
                                      (type)
                                      (lam
                                        v_2669
                                        (type)
                                        [
                                          List_2575
                                          [ [ Tuple2_2582 k_2668 ] v_2669 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                              (fun
                                [ List_2575 DCert_2650 ]
                                (fun
                                  [
                                    List_2575
                                    [
                                      [ Tuple2_2582 StakingCredential_2609 ]
                                      (con integer)
                                    ]
                                  ]
                                  (fun
                                    [ Interval_2645 (con integer) ]
                                    (fun
                                      [ List_2575 (con bytestring) ]
                                      (fun
                                        [
                                          List_2575
                                          [
                                            [ Tuple2_2582 (con bytestring) ]
                                            (con data)
                                          ]
                                        ]
                                        (fun (con bytestring) TxInfo_2659)
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl ScriptContext_2676 (type))

                    ScriptContext_match_2678
                    (vardecl
                      ScriptContext_2677
                      (fun
                        TxInfo_2659 (fun ScriptPurpose_2670 ScriptContext_2676)
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    mkPolicy_3392
                    (fun
                      TxOutRef_2624
                      (fun
                        (con bytestring)
                        (fun Unit_2563 (fun ScriptContext_2676 Bool_2527))
                      )
                    )
                  )
                  (lam
                    oref_3264
                    TxOutRef_2624
                    (lam
                      tn_3265
                      (con bytestring)
                      (lam
                        ds_3266
                        Unit_2563
                        (lam
                          ctx_3267
                          ScriptContext_2676
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl info_3272 TxInfo_2659)
                              [
                                {
                                  [ ScriptContext_match_2678 ctx_3267 ]
                                  TxInfo_2659
                                }
                                (lam
                                  ds_3270
                                  TxInfo_2659
                                  (lam ds_3271 ScriptPurpose_2670 ds_3270)
                                )
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl checkMintedAmount_3371 Bool_2527)
                              {
                                [
                                  [
                                    {
                                      [
                                        {
                                          Nil_match_2578
                                          [
                                            [
                                              [ Tuple3_3103 (con bytestring) ]
                                              (con bytestring)
                                            ]
                                            (con integer)
                                          ]
                                        }
                                        [
                                          flattenValue_3164
                                          [
                                            {
                                              [ TxInfo_match_2661 info_3272 ]
                                              [
                                                [
                                                  (lam
                                                    k_3327
                                                    (type)
                                                    (lam
                                                      v_3328
                                                      (type)
                                                      [
                                                        List_2575
                                                        [
                                                          [ Tuple2_2582 k_3327 ]
                                                          v_3328
                                                        ]
                                                      ]
                                                    )
                                                  )
                                                  (con bytestring)
                                                ]
                                                [
                                                  [
                                                    (lam
                                                      k_3329
                                                      (type)
                                                      (lam
                                                        v_3330
                                                        (type)
                                                        [
                                                          List_2575
                                                          [
                                                            [
                                                              Tuple2_2582 k_3329
                                                            ]
                                                            v_3330
                                                          ]
                                                        ]
                                                      )
                                                    )
                                                    (con bytestring)
                                                  ]
                                                  (con integer)
                                                ]
                                              ]
                                            }
                                            (lam
                                              ds_3331
                                              [ List_2575 TxInInfo_2627 ]
                                              (lam
                                                ds_3332
                                                [ List_2575 TxOut_2616 ]
                                                (lam
                                                  ds_3333
                                                  [
                                                    [
                                                      (lam
                                                        k_3334
                                                        (type)
                                                        (lam
                                                          v_3335
                                                          (type)
                                                          [
                                                            List_2575
                                                            [
                                                              [
                                                                Tuple2_2582
                                                                k_3334
                                                              ]
                                                              v_3335
                                                            ]
                                                          ]
                                                        )
                                                      )
                                                      (con bytestring)
                                                    ]
                                                    [
                                                      [
                                                        (lam
                                                          k_3336
                                                          (type)
                                                          (lam
                                                            v_3337
                                                            (type)
                                                            [
                                                              List_2575
                                                              [
                                                                [
                                                                  Tuple2_2582
                                                                  k_3336
                                                                ]
                                                                v_3337
                                                              ]
                                                            ]
                                                          )
                                                        )
                                                        (con bytestring)
                                                      ]
                                                      (con integer)
                                                    ]
                                                  ]
                                                  (lam
                                                    ds_3338
                                                    [
                                                      [
                                                        (lam
                                                          k_3339
                                                          (type)
                                                          (lam
                                                            v_3340
                                                            (type)
                                                            [
                                                              List_2575
                                                              [
                                                                [
                                                                  Tuple2_2582
                                                                  k_3339
                                                                ]
                                                                v_3340
                                                              ]
                                                            ]
                                                          )
                                                        )
                                                        (con bytestring)
                                                      ]
                                                      [
                                                        [
                                                          (lam
                                                            k_3341
                                                            (type)
                                                            (lam
                                                              v_3342
                                                              (type)
                                                              [
                                                                List_2575
                                                                [
                                                                  [
                                                                    Tuple2_2582
                                                                    k_3341
                                                                  ]
                                                                  v_3342
                                                                ]
                                                              ]
                                                            )
                                                          )
                                                          (con bytestring)
                                                        ]
                                                        (con integer)
                                                      ]
                                                    ]
                                                    (lam
                                                      ds_3343
                                                      [ List_2575 DCert_2650 ]
                                                      (lam
                                                        ds_3344
                                                        [
                                                          List_2575
                                                          [
                                                            [
                                                              Tuple2_2582
                                                              StakingCredential_2609
                                                            ]
                                                            (con integer)
                                                          ]
                                                        ]
                                                        (lam
                                                          ds_3345
                                                          [
                                                            Interval_2645
                                                            (con integer)
                                                          ]
                                                          (lam
                                                            ds_3346
                                                            [
                                                              List_2575
                                                              (con bytestring)
                                                            ]
                                                            (lam
                                                              ds_3347
                                                              [
                                                                List_2575
                                                                [
                                                                  [
                                                                    Tuple2_2582
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  ]
                                                                  (con data)
                                                                ]
                                                              ]
                                                              (lam
                                                                ds_3348
                                                                (con bytestring)
                                                                ds_3338
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          ]
                                        ]
                                      ]
                                      (all dead_3349 (type) Bool_2527)
                                    }
                                    (abs dead_3350 (type) False_2529)
                                  ]
                                  (lam
                                    ds_3351
                                    [
                                      [
                                        [ Tuple3_3103 (con bytestring) ]
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                    (lam
                                      ds_3352
                                      [
                                        List_2575
                                        [
                                          [
                                            [ Tuple3_3103 (con bytestring) ]
                                            (con bytestring)
                                          ]
                                          (con integer)
                                        ]
                                      ]
                                      (abs
                                        dead_3353
                                        (type)
                                        [
                                          {
                                            [
                                              {
                                                {
                                                  {
                                                    Tuple3_match_3105
                                                    (con bytestring)
                                                  }
                                                  (con bytestring)
                                                }
                                                (con integer)
                                              }
                                              ds_3351
                                            ]
                                            Bool_2527
                                          }
                                          (lam
                                            ds_3355
                                            (con bytestring)
                                            (lam
                                              tn_3356
                                              (con bytestring)
                                              (lam
                                                amt_3357
                                                (con integer)
                                                {
                                                  [
                                                    [
                                                      {
                                                        [
                                                          {
                                                            Nil_match_2578
                                                            [
                                                              [
                                                                [
                                                                  Tuple3_3103
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                ]
                                                                (con bytestring)
                                                              ]
                                                              (con integer)
                                                            ]
                                                          }
                                                          ds_3352
                                                        ]
                                                        (all
                                                          dead_3359
                                                          (type)
                                                          Bool_2527
                                                        )
                                                      }
                                                      (abs
                                                        dead_3360
                                                        (type)
                                                        {
                                                          [
                                                            [
                                                              {
                                                                [
                                                                  Bool_match_2530
                                                                  [
                                                                    [
                                                                      equalsByteString_3172
                                                                      tn_3356
                                                                    ]
                                                                    tn_3265
                                                                  ]
                                                                ]
                                                                (all
                                                                  dead_3362
                                                                  (type)
                                                                  Bool_2527
                                                                )
                                                              }
                                                              (abs
                                                                dead_3363
                                                                (type)
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        ifThenElse_2686
                                                                        Bool_2527
                                                                      }
                                                                      [
                                                                        [
                                                                          equalsInteger_2693
                                                                          amt_3357
                                                                        ]
                                                                        (con
                                                                          integer
                                                                          1
                                                                        )
                                                                      ]
                                                                    ]
                                                                    True_2528
                                                                  ]
                                                                  False_2529
                                                                ]
                                                              )
                                                            ]
                                                            (abs
                                                              dead_3364
                                                              (type)
                                                              False_2529
                                                            )
                                                          ]
                                                          (all
                                                            dead_3365
                                                            (type)
                                                            dead_3365
                                                          )
                                                        }
                                                      )
                                                    ]
                                                    (lam
                                                      ipv_3366
                                                      [
                                                        [
                                                          [
                                                            Tuple3_3103
                                                            (con bytestring)
                                                          ]
                                                          (con bytestring)
                                                        ]
                                                        (con integer)
                                                      ]
                                                      (lam
                                                        ipv_3367
                                                        [
                                                          List_2575
                                                          [
                                                            [
                                                              [
                                                                Tuple3_3103
                                                                (con bytestring)
                                                              ]
                                                              (con bytestring)
                                                            ]
                                                            (con integer)
                                                          ]
                                                        ]
                                                        (abs
                                                          dead_3368
                                                          (type)
                                                          False_2529
                                                        )
                                                      )
                                                    )
                                                  ]
                                                  (all
                                                    dead_3369 (type) dead_3369
                                                  )
                                                }
                                              )
                                            )
                                          )
                                        ]
                                      )
                                    )
                                  )
                                ]
                                (all dead_3370 (type) dead_3370)
                              }
                            )
                            (termbind
                              (nonstrict)
                              (vardecl
                                hasUTxO_3299
                                [ (lam a_3300 (type) a_3300) Bool_2527 ]
                              )
                              [
                                [
                                  [
                                    {
                                      {
                                        fFoldableNil_cfoldMap_3220
                                        [ (lam a_3275 (type) a_3275) Bool_2527 ]
                                      }
                                      TxInInfo_2627
                                    }
                                    dMonoid_3273
                                  ]
                                  (lam
                                    i_3276
                                    TxInInfo_2627
                                    [
                                      [
                                        fEqTxOutRef_c_3262
                                        [
                                          {
                                            [ TxInInfo_match_2629 i_3276 ]
                                            TxOutRef_2624
                                          }
                                          (lam
                                            ds_3278
                                            TxOutRef_2624
                                            (lam ds_3279 TxOut_2616 ds_3278)
                                          )
                                        ]
                                      ]
                                      oref_3264
                                    ]
                                  )
                                ]
                                [
                                  {
                                    [ TxInfo_match_2661 info_3272 ]
                                    [ List_2575 TxInInfo_2627 ]
                                  }
                                  (lam
                                    ds_3281
                                    [ List_2575 TxInInfo_2627 ]
                                    (lam
                                      ds_3282
                                      [ List_2575 TxOut_2616 ]
                                      (lam
                                        ds_3283
                                        [
                                          [
                                            (lam
                                              k_3284
                                              (type)
                                              (lam
                                                v_3285
                                                (type)
                                                [
                                                  List_2575
                                                  [
                                                    [ Tuple2_2582 k_3284 ]
                                                    v_3285
                                                  ]
                                                ]
                                              )
                                            )
                                            (con bytestring)
                                          ]
                                          [
                                            [
                                              (lam
                                                k_3286
                                                (type)
                                                (lam
                                                  v_3287
                                                  (type)
                                                  [
                                                    List_2575
                                                    [
                                                      [ Tuple2_2582 k_3286 ]
                                                      v_3287
                                                    ]
                                                  ]
                                                )
                                              )
                                              (con bytestring)
                                            ]
                                            (con integer)
                                          ]
                                        ]
                                        (lam
                                          ds_3288
                                          [
                                            [
                                              (lam
                                                k_3289
                                                (type)
                                                (lam
                                                  v_3290
                                                  (type)
                                                  [
                                                    List_2575
                                                    [
                                                      [ Tuple2_2582 k_3289 ]
                                                      v_3290
                                                    ]
                                                  ]
                                                )
                                              )
                                              (con bytestring)
                                            ]
                                            [
                                              [
                                                (lam
                                                  k_3291
                                                  (type)
                                                  (lam
                                                    v_3292
                                                    (type)
                                                    [
                                                      List_2575
                                                      [
                                                        [ Tuple2_2582 k_3291 ]
                                                        v_3292
                                                      ]
                                                    ]
                                                  )
                                                )
                                                (con bytestring)
                                              ]
                                              (con integer)
                                            ]
                                          ]
                                          (lam
                                            ds_3293
                                            [ List_2575 DCert_2650 ]
                                            (lam
                                              ds_3294
                                              [
                                                List_2575
                                                [
                                                  [
                                                    Tuple2_2582
                                                    StakingCredential_2609
                                                  ]
                                                  (con integer)
                                                ]
                                              ]
                                              (lam
                                                ds_3295
                                                [ Interval_2645 (con integer) ]
                                                (lam
                                                  ds_3296
                                                  [ List_2575 (con bytestring) ]
                                                  (lam
                                                    ds_3297
                                                    [
                                                      List_2575
                                                      [
                                                        [
                                                          Tuple2_2582
                                                          (con bytestring)
                                                        ]
                                                        (con data)
                                                      ]
                                                    ]
                                                    (lam
                                                      ds_3298
                                                      (con bytestring)
                                                      ds_3281
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl hasUTxO_3301 Bool_2527)
                              hasUTxO_3299
                            )
                            [
                              { [ Unit_match_2565 ds_3266 ] Bool_2527 }
                              {
                                [
                                  [
                                    {
                                      [
                                        Bool_match_2530
                                        {
                                          [
                                            [
                                              {
                                                [ Bool_match_2530 hasUTxO_3301 ]
                                                (all dead_3379 (type) Bool_2527)
                                              }
                                              (abs dead_3380 (type) True_2528)
                                            ]
                                            (abs
                                              dead_3381
                                              (type)
                                              [
                                                [
                                                  { trace_2555 Bool_2527 }
                                                  (con
                                                    string "UTxO not consumed"
                                                  )
                                                ]
                                                False_2529
                                              ]
                                            )
                                          ]
                                          (all dead_3382 (type) dead_3382)
                                        }
                                      ]
                                      (all dead_3383 (type) Bool_2527)
                                    }
                                    (abs
                                      dead_3384
                                      (type)
                                      {
                                        [
                                          [
                                            {
                                              [
                                                Bool_match_2530
                                                checkMintedAmount_3371
                                              ]
                                              (all dead_3386 (type) Bool_2527)
                                            }
                                            (abs dead_3387 (type) True_2528)
                                          ]
                                          (abs
                                            dead_3388
                                            (type)
                                            [
                                              [
                                                { trace_2555 Bool_2527 }
                                                (con
                                                  string "wrong amount minted"
                                                )
                                              ]
                                              False_2529
                                            ]
                                          )
                                        ]
                                        (all dead_3389 (type) dead_3389)
                                      }
                                    )
                                  ]
                                  (abs dead_3390 (type) False_2529)
                                ]
                                (all dead_3391 (type) dead_3391)
                              }
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3074 ScriptContext_2676)
                  [
                    { error_2559 ScriptContext_2676 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3057 ScriptPurpose_2670)
                  [
                    { error_2559 ScriptPurpose_2670 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2964 DCert_2650)
                  [
                    { error_2559 DCert_2650 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2793 StakingCredential_2609)
                  [
                    { error_2559 StakingCredential_2609 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2774 Credential_2605)
                  [
                    { error_2559 Credential_2605 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl unsafeDataAsB_2697 (fun (con data) (con bytestring)))
                  (builtin unBData)
                )
                (termbind
                  (strict)
                  (vardecl
                    head_2688
                    (all a_2689 (type) (fun [ (con list) a_2689 ] a_2689))
                  )
                  (builtin headList)
                )
                (termbind
                  (strict)
                  (vardecl
                    snd_2682
                    (all
                      a_2683
                      (type)
                      (all
                        b_2684
                        (type)
                        (fun [ [ (con pair) a_2683 ] b_2684 ] b_2684)
                      )
                    )
                  )
                  (builtin sndPair)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataCredential_cunsafeFromBuiltinData_2778
                    (fun (con data) Credential_2605)
                  )
                  (lam
                    d_2764
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2765
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2764 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2770 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2765
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2771 Credential_2605)
                        [ PubKeyCredential_2606 x_2770 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_2766 (con integer))
                        [
                          {
                            { fst_2690 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2765
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2776 Credential_2605)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2686
                                  (fun (con unit) Credential_2605)
                                }
                                [
                                  [ equalsInteger_2693 index_2766 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2772 (con unit) x_2771)
                            ]
                            (lam ds_2775 (con unit) x_2774)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2767 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2765
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2768 Credential_2605)
                        [ ScriptCredential_2607 x_2767 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2686 (fun (con unit) Credential_2605)
                              }
                              [
                                [ equalsInteger_2693 index_2766 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2769 (con unit) x_2768)
                          ]
                          (lam ds_2777 (con unit) x_2776)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl unsafeDataAsI_2696 (fun (con data) (con integer)))
                  (builtin unIData)
                )
                (termbind
                  (strict)
                  (vardecl
                    tail_2680
                    (all
                      a_2681
                      (type)
                      (fun [ (con list) a_2681 ] [ (con list) a_2681 ])
                    )
                  )
                  (builtin tailList)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                    (fun (con data) StakingCredential_2609)
                  )
                  (lam
                    d_2779
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2780
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2779 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2789 Credential_2605)
                        [
                          fUnsafeFromDataCredential_cunsafeFromBuiltinData_2778
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2780
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2790 StakingCredential_2609)
                        [ StakingHash_2610 x_2789 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_2781 (con integer))
                        [
                          {
                            { fst_2690 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2780
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2795 StakingCredential_2609)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2686
                                  (fun (con unit) StakingCredential_2609)
                                }
                                [
                                  [ equalsInteger_2693 index_2781 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2791 (con unit) x_2790)
                            ]
                            (lam ds_2794 (con unit) x_2793)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2782 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2780
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2783 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_2782 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2786 (con integer))
                        [
                          unsafeDataAsI_2696
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2783 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2785 (con integer))
                        [
                          unsafeDataAsI_2696 [ { head_2688 (con data) } t_2783 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2784 (con integer))
                        [
                          unsafeDataAsI_2696 [ { head_2688 (con data) } t_2782 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2787 StakingCredential_2609)
                        [ [ [ StakingPtr_2611 x_2784 ] x_2785 ] x_2786 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2686
                                (fun (con unit) StakingCredential_2609)
                              }
                              [
                                [ equalsInteger_2693 index_2781 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2788 (con unit) x_2787)
                          ]
                          (lam ds_2796 (con unit) x_2795)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataDCert_cunsafeFromBuiltinData_2978
                    (fun (con data) DCert_2650)
                  )
                  (lam
                    d_2937
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2938
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2937 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2960 StakingCredential_2609)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2938
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2961 DCert_2650)
                        [ DCertDelegRegKey_2653 x_2960 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_2939 (con integer))
                        [
                          {
                            { fst_2690 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2938
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2966 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2962 (con unit) x_2961)
                            ]
                            (lam ds_2965 (con unit) x_2964)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2957 StakingCredential_2609)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2938
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2958 DCert_2650)
                        [ DCertDelegDeRegKey_2651 x_2957 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2968 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 1)
                                ]
                              ]
                              (lam ds_2959 (con unit) x_2958)
                            ]
                            (lam ds_2967 (con unit) x_2966)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2952 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2938
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2954 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2952 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2953 StakingCredential_2609)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                          [ { head_2688 (con data) } t_2952 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2955 DCert_2650)
                        [ [ DCertDelegDelegate_2652 x_2953 ] x_2954 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2970 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 2)
                                ]
                              ]
                              (lam ds_2956 (con unit) x_2955)
                            ]
                            (lam ds_2969 (con unit) x_2968)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2947 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2938
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2949 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2947 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2948 (con bytestring))
                        [
                          unsafeDataAsB_2697 [ { head_2688 (con data) } t_2947 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2950 DCert_2650)
                        [ [ DCertPoolRegister_2656 x_2948 ] x_2949 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2972 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 3)
                                ]
                              ]
                              (lam ds_2951 (con unit) x_2950)
                            ]
                            (lam ds_2971 (con unit) x_2970)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2942 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2938
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2944 (con integer))
                        [
                          unsafeDataAsI_2696
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2942 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2943 (con bytestring))
                        [
                          unsafeDataAsB_2697 [ { head_2688 (con data) } t_2942 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2945 DCert_2650)
                        [ [ DCertPoolRetire_2657 x_2943 ] x_2944 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2974 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 4)
                                ]
                              ]
                              (lam ds_2946 (con unit) x_2945)
                            ]
                            (lam ds_2973 (con unit) x_2972)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2976 DCert_2650)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) DCert_2650) }
                                [
                                  [ equalsInteger_2693 index_2939 ]
                                  (con integer 5)
                                ]
                              ]
                              (lam ds_2941 (con unit) DCertGenesis_2654)
                            ]
                            (lam ds_2975 (con unit) x_2974)
                          ]
                          unitval_2554
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) DCert_2650) }
                              [
                                [ equalsInteger_2693 index_2939 ]
                                (con integer 6)
                              ]
                            ]
                            (lam ds_2940 (con unit) DCertMir_2655)
                          ]
                          (lam ds_2977 (con unit) x_2976)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2844 TxOutRef_2624)
                  [
                    { error_2559 TxOutRef_2624 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2833 (con bytestring))
                  [
                    { error_2559 (con bytestring) }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxId_cunsafeFromBuiltinData_2835
                    (fun (con data) (con bytestring))
                  )
                  (lam
                    d_2828
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2829
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2828 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2830 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2829
                            ]
                          ]
                        ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2686
                                (fun (con unit) (con bytestring))
                              }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2829
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2831 (con unit) x_2830)
                          ]
                          (lam ds_2834 (con unit) x_2833)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2846
                    (fun (con data) TxOutRef_2624)
                  )
                  (lam
                    d_2836
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2837
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2836 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2838 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2837
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2840 (con integer))
                        [
                          unsafeDataAsI_2696
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2838 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2839 (con bytestring))
                        [
                          fUnsafeFromDataTxId_cunsafeFromBuiltinData_2835
                          [ { head_2688 (con data) } t_2838 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2841 TxOutRef_2624)
                        [ [ TxOutRef_2625 x_2839 ] x_2840 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) TxOutRef_2624) }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2837
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2842 (con unit) x_2841)
                          ]
                          (lam ds_2845 (con unit) x_2844)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3065
                    (fun (con data) ScriptPurpose_2670)
                  )
                  (lam
                    d_3041
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3042
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_3041 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3053 (con bytestring))
                        [
                          unsafeDataAsB_2697
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3042
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3054 ScriptPurpose_2670)
                        [ Minting_2672 x_3053 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_3043 (con integer))
                        [
                          {
                            { fst_2690 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3042
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3059 ScriptPurpose_2670)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2686
                                  (fun (con unit) ScriptPurpose_2670)
                                }
                                [
                                  [ equalsInteger_2693 index_3043 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_3055 (con unit) x_3054)
                            ]
                            (lam ds_3058 (con unit) x_3057)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3050 TxOutRef_2624)
                        [
                          fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2846
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3042
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3051 ScriptPurpose_2670)
                        [ Spending_2674 x_3050 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3061 ScriptPurpose_2670)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2686
                                  (fun (con unit) ScriptPurpose_2670)
                                }
                                [
                                  [ equalsInteger_2693 index_3043 ]
                                  (con integer 1)
                                ]
                              ]
                              (lam ds_3052 (con unit) x_3051)
                            ]
                            (lam ds_3060 (con unit) x_3059)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3047 StakingCredential_2609)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3042
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3048 ScriptPurpose_2670)
                        [ Rewarding_2673 x_3047 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3063 ScriptPurpose_2670)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2686
                                  (fun (con unit) ScriptPurpose_2670)
                                }
                                [
                                  [ equalsInteger_2693 index_3043 ]
                                  (con integer 2)
                                ]
                              ]
                              (lam ds_3049 (con unit) x_3048)
                            ]
                            (lam ds_3062 (con unit) x_3061)
                          ]
                          unitval_2554
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3044 DCert_2650)
                        [
                          fUnsafeFromDataDCert_cunsafeFromBuiltinData_2978
                          [
                            { head_2688 (con data) }
                            [
                              {
                                { snd_2682 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3042
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3045 ScriptPurpose_2670)
                        [ Certifying_2671 x_3044 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2686
                                (fun (con unit) ScriptPurpose_2670)
                              }
                              [
                                [ equalsInteger_2693 index_3043 ]
                                (con integer 3)
                              ]
                            ]
                            (lam ds_3046 (con unit) x_3045)
                          ]
                          (lam ds_3064 (con unit) x_3063)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3038 TxInfo_2659)
                  [
                    { error_2559 TxInfo_2659 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2997
                    (all
                      a_2998
                      (type)
                      (all
                        b_2999
                        (type)
                        (fun
                          [ (lam a_3000 (type) (fun (con data) a_3000)) a_2998 ]
                          (fun
                            [
                              (lam a_3001 (type) (fun (con data) a_3001)) b_2999
                            ]
                            (fun (con data) [ [ Tuple2_2582 a_2998 ] b_2999 ])
                          )
                        )
                      )
                    )
                  )
                  (abs
                    a_2981
                    (type)
                    (abs
                      b_2982
                      (type)
                      (let
                        (nonrec)
                        (termbind
                          (nonstrict)
                          (vardecl x_2995 [ [ Tuple2_2582 a_2981 ] b_2982 ])
                          [
                            { error_2559 [ [ Tuple2_2582 a_2981 ] b_2982 ] }
                            [
                              {
                                [
                                  Unit_match_2565
                                  [
                                    [
                                      { trace_2555 Unit_2563 }
                                      reconstructCaseError_2685
                                    ]
                                    Unit_2564
                                  ]
                                ]
                                (con unit)
                              }
                              unitval_2554
                            ]
                          ]
                        )
                        (lam
                          dUnsafeFromData_2983
                          [ (lam a_2984 (type) (fun (con data) a_2984)) a_2981 ]
                          (lam
                            dUnsafeFromData_2985
                            [
                              (lam a_2986 (type) (fun (con data) a_2986)) b_2982
                            ]
                            (lam
                              d_2987
                              (con data)
                              (let
                                (nonrec)
                                (termbind
                                  (nonstrict)
                                  (vardecl
                                    tup_2988
                                    [
                                      [ (con pair) (con integer) ]
                                      [ (con list) (con data) ]
                                    ]
                                  )
                                  [ unsafeDataAsConstr_2679 d_2987 ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl t_2989 [ (con list) (con data) ])
                                  [
                                    {
                                      { snd_2682 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2988
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl x_2991 b_2982)
                                  [
                                    dUnsafeFromData_2985
                                    [
                                      { head_2688 (con data) }
                                      [ { tail_2680 (con data) } t_2989 ]
                                    ]
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl x_2990 a_2981)
                                  [
                                    dUnsafeFromData_2983
                                    [ { head_2688 (con data) } t_2989 ]
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl
                                    x_2992 [ [ Tuple2_2582 a_2981 ] b_2982 ]
                                  )
                                  [
                                    [ { { Tuple2_2583 a_2981 } b_2982 } x_2990 ]
                                    x_2991
                                  ]
                                )
                                [
                                  [
                                    [
                                      [
                                        {
                                          ifThenElse_2686
                                          (fun
                                            (con unit)
                                            [ [ Tuple2_2582 a_2981 ] b_2982 ]
                                          )
                                        }
                                        [
                                          [
                                            equalsInteger_2693
                                            [
                                              {
                                                { fst_2690 (con integer) }
                                                [ (con list) (con data) ]
                                              }
                                              tup_2988
                                            ]
                                          ]
                                          (con integer 0)
                                        ]
                                      ]
                                      (lam ds_2993 (con unit) x_2992)
                                    ]
                                    (lam ds_2996 (con unit) x_2995)
                                  ]
                                  unitval_2554
                                ]
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_2980
                    (fun (con data) (con data))
                  )
                  (lam d_2979 (con data) d_2979)
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2884 Bool_2527)
                  [
                    { error_2559 Bool_2527 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataBool_cunsafeFromBuiltinData_2888
                    (fun (con data) Bool_2527)
                  )
                  (lam
                    d_2879
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl index_2880 (con integer))
                        [
                          {
                            { fst_2690 (con integer) } [ (con list) (con data) ]
                          }
                          [ unsafeDataAsConstr_2679 d_2879 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2886 Bool_2527)
                        [
                          [
                            [
                              [
                                { ifThenElse_2686 (fun (con unit) Bool_2527) }
                                [
                                  [ equalsInteger_2693 index_2880 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2882 (con unit) False_2529)
                            ]
                            (lam ds_2885 (con unit) x_2884)
                          ]
                          unitval_2554
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) Bool_2527) }
                              [
                                [ equalsInteger_2693 index_2880 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2881 (con unit) True_2528)
                          ]
                          (lam ds_2887 (con unit) x_2886)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_2876
                    (all
                      a_2877
                      (type)
                      (fun
                        [ (lam a_2878 (type) (fun (con data) a_2878)) a_2877 ]
                        (fun (con data) [ Extended_2631 a_2877 ])
                      )
                    )
                  )
                  (abs
                    a_2858
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2870 [ Extended_2631 a_2858 ])
                        [
                          { error_2559 [ Extended_2631 a_2858 ] }
                          [
                            {
                              [
                                Unit_match_2565
                                [
                                  [
                                    { trace_2555 Unit_2563 }
                                    reconstructCaseError_2685
                                  ]
                                  Unit_2564
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2554
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2859
                        [ (lam a_2860 (type) (fun (con data) a_2860)) a_2858 ]
                        (lam
                          d_2861
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2862
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2679 d_2861 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl index_2863 (con integer))
                              [
                                {
                                  { fst_2690 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2862
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2872 [ Extended_2631 a_2858 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2686
                                        (fun
                                          (con unit) [ Extended_2631 a_2858 ]
                                        )
                                      }
                                      [
                                        [ equalsInteger_2693 index_2863 ]
                                        (con integer 0)
                                      ]
                                    ]
                                    (lam
                                      ds_2868 (con unit) { NegInf_2633 a_2858 }
                                    )
                                  ]
                                  (lam ds_2871 (con unit) x_2870)
                                ]
                                unitval_2554
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2865 a_2858)
                              [
                                dUnsafeFromData_2859
                                [
                                  { head_2688 (con data) }
                                  [
                                    {
                                      { snd_2682 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2862
                                  ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2866 [ Extended_2631 a_2858 ])
                              [ { Finite_2632 a_2858 } x_2865 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2874 [ Extended_2631 a_2858 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2686
                                        (fun
                                          (con unit) [ Extended_2631 a_2858 ]
                                        )
                                      }
                                      [
                                        [ equalsInteger_2693 index_2863 ]
                                        (con integer 1)
                                      ]
                                    ]
                                    (lam ds_2867 (con unit) x_2866)
                                  ]
                                  (lam ds_2873 (con unit) x_2872)
                                ]
                                unitval_2554
                              ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2686
                                      (fun (con unit) [ Extended_2631 a_2858 ])
                                    }
                                    [
                                      [ equalsInteger_2693 index_2863 ]
                                      (con integer 2)
                                    ]
                                  ]
                                  (lam
                                    ds_2864 (con unit) { PosInf_2634 a_2858 }
                                  )
                                ]
                                (lam ds_2875 (con unit) x_2874)
                              ]
                              unitval_2554
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_2918
                    (all
                      a_2919
                      (type)
                      (fun
                        [ (lam a_2920 (type) (fun (con data) a_2920)) a_2919 ]
                        (fun (con data) [ UpperBound_2637 a_2919 ])
                      )
                    )
                  )
                  (abs
                    a_2905
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2916 [ UpperBound_2637 a_2905 ])
                        [
                          { error_2559 [ UpperBound_2637 a_2905 ] }
                          [
                            {
                              [
                                Unit_match_2565
                                [
                                  [
                                    { trace_2555 Unit_2563 }
                                    reconstructCaseError_2685
                                  ]
                                  Unit_2564
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2554
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2906
                        [ (lam a_2907 (type) (fun (con data) a_2907)) a_2905 ]
                        (lam
                          d_2908
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2909
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2679 d_2908 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_2910 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2682 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2909
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2912 Bool_2527)
                              [
                                fUnsafeFromDataBool_cunsafeFromBuiltinData_2888
                                [
                                  { head_2688 (con data) }
                                  [ { tail_2680 (con data) } t_2910 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2911 [ Extended_2631 a_2905 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_2876
                                    a_2905
                                  }
                                  dUnsafeFromData_2906
                                ]
                                [ { head_2688 (con data) } t_2910 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2913 [ UpperBound_2637 a_2905 ])
                              [ [ { UpperBound_2638 a_2905 } x_2911 ] x_2912 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2686
                                      (fun
                                        (con unit) [ UpperBound_2637 a_2905 ]
                                      )
                                    }
                                    [
                                      [
                                        equalsInteger_2693
                                        [
                                          {
                                            { fst_2690 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_2909
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_2914 (con unit) x_2913)
                                ]
                                (lam ds_2917 (con unit) x_2916)
                              ]
                              unitval_2554
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_2902
                    (all
                      a_2903
                      (type)
                      (fun
                        [ (lam a_2904 (type) (fun (con data) a_2904)) a_2903 ]
                        (fun (con data) [ LowerBound_2641 a_2903 ])
                      )
                    )
                  )
                  (abs
                    a_2889
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2900 [ LowerBound_2641 a_2889 ])
                        [
                          { error_2559 [ LowerBound_2641 a_2889 ] }
                          [
                            {
                              [
                                Unit_match_2565
                                [
                                  [
                                    { trace_2555 Unit_2563 }
                                    reconstructCaseError_2685
                                  ]
                                  Unit_2564
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2554
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2890
                        [ (lam a_2891 (type) (fun (con data) a_2891)) a_2889 ]
                        (lam
                          d_2892
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2893
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2679 d_2892 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_2894 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2682 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2893
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2896 Bool_2527)
                              [
                                fUnsafeFromDataBool_cunsafeFromBuiltinData_2888
                                [
                                  { head_2688 (con data) }
                                  [ { tail_2680 (con data) } t_2894 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2895 [ Extended_2631 a_2889 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_2876
                                    a_2889
                                  }
                                  dUnsafeFromData_2890
                                ]
                                [ { head_2688 (con data) } t_2894 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2897 [ LowerBound_2641 a_2889 ])
                              [ [ { LowerBound_2642 a_2889 } x_2895 ] x_2896 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2686
                                      (fun
                                        (con unit) [ LowerBound_2641 a_2889 ]
                                      )
                                    }
                                    [
                                      [
                                        equalsInteger_2693
                                        [
                                          {
                                            { fst_2690 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_2893
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_2898 (con unit) x_2897)
                                ]
                                (lam ds_2901 (con unit) x_2900)
                              ]
                              unitval_2554
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_2934
                    (all
                      a_2935
                      (type)
                      (fun
                        [ (lam a_2936 (type) (fun (con data) a_2936)) a_2935 ]
                        (fun (con data) [ Interval_2645 a_2935 ])
                      )
                    )
                  )
                  (abs
                    a_2921
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2932 [ Interval_2645 a_2921 ])
                        [
                          { error_2559 [ Interval_2645 a_2921 ] }
                          [
                            {
                              [
                                Unit_match_2565
                                [
                                  [
                                    { trace_2555 Unit_2563 }
                                    reconstructCaseError_2685
                                  ]
                                  Unit_2564
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2554
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2922
                        [ (lam a_2923 (type) (fun (con data) a_2923)) a_2921 ]
                        (lam
                          d_2924
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2925
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2679 d_2924 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_2926 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2682 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2925
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2928 [ UpperBound_2637 a_2921 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_2918
                                    a_2921
                                  }
                                  dUnsafeFromData_2922
                                ]
                                [
                                  { head_2688 (con data) }
                                  [ { tail_2680 (con data) } t_2926 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2927 [ LowerBound_2641 a_2921 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_2902
                                    a_2921
                                  }
                                  dUnsafeFromData_2922
                                ]
                                [ { head_2688 (con data) } t_2926 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2929 [ Interval_2645 a_2921 ])
                              [ [ { Interval_2646 a_2921 } x_2927 ] x_2928 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2686
                                      (fun (con unit) [ Interval_2645 a_2921 ])
                                    }
                                    [
                                      [
                                        equalsInteger_2693
                                        [
                                          {
                                            { fst_2690 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_2925
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_2930 (con unit) x_2929)
                                ]
                                (lam ds_2933 (con unit) x_2932)
                              ]
                              unitval_2554
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2855 TxInInfo_2627)
                  [
                    { error_2559 TxInInfo_2627 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2825 TxOut_2616)
                  [
                    { error_2559 TxOut_2616 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2806 Address_2613)
                  [
                    { error_2559 Address_2613 }
                    [
                      {
                        [
                          Unit_match_2565
                          [
                            [
                              { trace_2555 Unit_2563 } reconstructCaseError_2685
                            ]
                            Unit_2564
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2554
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2761
                    (all
                      a_2762
                      (type)
                      (fun
                        [ (lam a_2763 (type) (fun (con data) a_2763)) a_2762 ]
                        (fun (con data) [ Maybe_2597 a_2762 ])
                      )
                    )
                  )
                  (abs
                    a_2746
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2757 [ Maybe_2597 a_2746 ])
                        [
                          { error_2559 [ Maybe_2597 a_2746 ] }
                          [
                            {
                              [
                                Unit_match_2565
                                [
                                  [
                                    { trace_2555 Unit_2563 }
                                    reconstructCaseError_2685
                                  ]
                                  Unit_2564
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2554
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2747
                        [ (lam a_2748 (type) (fun (con data) a_2748)) a_2746 ]
                        (lam
                          d_2749
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2750
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2679 d_2749 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl index_2751 (con integer))
                              [
                                {
                                  { fst_2690 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2750
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2759 [ Maybe_2597 a_2746 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2686
                                        (fun (con unit) [ Maybe_2597 a_2746 ])
                                      }
                                      [
                                        [ equalsInteger_2693 index_2751 ]
                                        (con integer 1)
                                      ]
                                    ]
                                    (lam
                                      ds_2755 (con unit) { Nothing_2599 a_2746 }
                                    )
                                  ]
                                  (lam ds_2758 (con unit) x_2757)
                                ]
                                unitval_2554
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2752 a_2746)
                              [
                                dUnsafeFromData_2747
                                [
                                  { head_2688 (con data) }
                                  [
                                    {
                                      { snd_2682 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2750
                                  ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2753 [ Maybe_2597 a_2746 ])
                              [ { Just_2598 a_2746 } x_2752 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2686
                                      (fun (con unit) [ Maybe_2597 a_2746 ])
                                    }
                                    [
                                      [ equalsInteger_2693 index_2751 ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_2754 (con unit) x_2753)
                                ]
                                (lam ds_2760 (con unit) x_2759)
                              ]
                              unitval_2554
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataAddress_cunsafeFromBuiltinData_2808
                    (fun (con data) Address_2613)
                  )
                  (lam
                    d_2798
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2799
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2798 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2800 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2799
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2802 [ Maybe_2597 StakingCredential_2609 ])
                        [
                          [
                            {
                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2761
                              StakingCredential_2609
                            }
                            fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                          ]
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2800 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2801 Credential_2605)
                        [
                          fUnsafeFromDataCredential_cunsafeFromBuiltinData_2778
                          [ { head_2688 (con data) } t_2800 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2803 Address_2613)
                        [ [ Address_2614 x_2801 ] x_2802 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) Address_2613) }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2799
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2804 (con unit) x_2803)
                          ]
                          (lam ds_2807 (con unit) x_2806)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMap_2717
                    (all
                      v_2718
                      (type)
                      (all
                        k_2719
                        (type)
                        (fun
                          Unit_2563
                          [ List_2575 [ [ Tuple2_2582 k_2719 ] v_2718 ] ]
                        )
                      )
                    )
                  )
                  (abs
                    v_2714
                    (type)
                    (abs
                      k_2715
                      (type)
                      (lam
                        ds_2716
                        Unit_2563
                        { Nil_2576 [ [ Tuple2_2582 k_2715 ] v_2714 ] }
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    unsafeDataAsMap_2713
                    (fun
                      (con data)
                      [ (con list) [ [ (con pair) (con data) ] (con data) ] ]
                    )
                  )
                  (builtin unMapData)
                )
                (termbind
                  (strict)
                  (vardecl
                    chooseList_2699
                    (all
                      a_2700
                      (type)
                      (all
                        b_2701
                        (type)
                        (fun
                          [ (con list) a_2700 ] (fun b_2701 (fun b_2701 b_2701))
                        )
                      )
                    )
                  )
                  (builtin chooseList)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMap_cunsafeFromBuiltinData_2736
                    (all
                      k_2737
                      (type)
                      (all
                        v_2738
                        (type)
                        (fun
                          [ (lam a_2739 (type) (fun (con data) a_2739)) k_2737 ]
                          (fun
                            [
                              (lam a_2740 (type) (fun (con data) a_2740)) v_2738
                            ]
                            (fun
                              (con data)
                              [
                                [
                                  (lam
                                    k_2741
                                    (type)
                                    (lam
                                      v_2742
                                      (type)
                                      [
                                        List_2575
                                        [ [ Tuple2_2582 k_2741 ] v_2742 ]
                                      ]
                                    )
                                  )
                                  k_2737
                                ]
                                v_2738
                              ]
                            )
                          )
                        )
                      )
                    )
                  )
                  (abs
                    k_2720
                    (type)
                    (abs
                      v_2721
                      (type)
                      (lam
                        dUnsafeFromData_2722
                        [ (lam a_2723 (type) (fun (con data) a_2723)) k_2720 ]
                        (lam
                          dUnsafeFromData_2724
                          [ (lam a_2725 (type) (fun (con data) a_2725)) v_2721 ]
                          (let
                            (rec)
                            (termbind
                              (strict)
                              (vardecl
                                go_2727
                                (fun
                                  [
                                    (con list)
                                    [ [ (con pair) (con data) ] (con data) ]
                                  ]
                                  [
                                    List_2575 [ [ Tuple2_2582 k_2720 ] v_2721 ]
                                  ]
                                )
                              )
                              (lam
                                l_2728
                                [
                                  (con list)
                                  [ [ (con pair) (con data) ] (con data) ]
                                ]
                                (let
                                  (nonrec)
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2733
                                      [
                                        List_2575
                                        [ [ Tuple2_2582 k_2720 ] v_2721 ]
                                      ]
                                    )
                                    [
                                      go_2727
                                      [
                                        {
                                          tail_2680
                                          [
                                            [ (con pair) (con data) ] (con data)
                                          ]
                                        }
                                        l_2728
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      tup_2729
                                      [ [ (con pair) (con data) ] (con data) ]
                                    )
                                    [
                                      {
                                        head_2688
                                        [ [ (con pair) (con data) ] (con data) ]
                                      }
                                      l_2728
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl lvl_2731 v_2721)
                                    [
                                      dUnsafeFromData_2724
                                      [
                                        { { snd_2682 (con data) } (con data) }
                                        tup_2729
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl lvl_2730 k_2720)
                                    [
                                      dUnsafeFromData_2722
                                      [
                                        { { fst_2690 (con data) } (con data) }
                                        tup_2729
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2732 [ [ Tuple2_2582 k_2720 ] v_2721 ]
                                    )
                                    [
                                      [
                                        { { Tuple2_2583 k_2720 } v_2721 }
                                        lvl_2730
                                      ]
                                      lvl_2731
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2734
                                      [
                                        List_2575
                                        [ [ Tuple2_2582 k_2720 ] v_2721 ]
                                      ]
                                    )
                                    [
                                      [
                                        {
                                          Cons_2577
                                          [ [ Tuple2_2582 k_2720 ] v_2721 ]
                                        }
                                        lvl_2732
                                      ]
                                      lvl_2733
                                    ]
                                  )
                                  [
                                    [
                                      [
                                        [
                                          {
                                            {
                                              chooseList_2699
                                              [
                                                [ (con pair) (con data) ]
                                                (con data)
                                              ]
                                            }
                                            (fun
                                              Unit_2563
                                              [
                                                List_2575
                                                [
                                                  [ Tuple2_2582 k_2720 ] v_2721
                                                ]
                                              ]
                                            )
                                          }
                                          l_2728
                                        ]
                                        {
                                          { fUnsafeFromDataMap_2717 v_2721 }
                                          k_2720
                                        }
                                      ]
                                      (lam ds_2735 Unit_2563 lvl_2734)
                                    ]
                                    Unit_2564
                                  ]
                                )
                              )
                            )
                            (lam
                              d_2726
                              (con data)
                              [ go_2727 [ unsafeDataAsMap_2713 d_2726 ] ]
                            )
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl
                    fUnsafeFromDataValue_2743
                    (fun
                      (con data)
                      [
                        [
                          (lam
                            k_2744
                            (type)
                            (lam
                              v_2745
                              (type)
                              [ List_2575 [ [ Tuple2_2582 k_2744 ] v_2745 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        (con integer)
                      ]
                    )
                  )
                  [
                    [
                      {
                        {
                          fUnsafeFromDataMap_cunsafeFromBuiltinData_2736
                          (con bytestring)
                        }
                        (con integer)
                      }
                      unsafeDataAsB_2697
                    ]
                    unsafeDataAsI_2696
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2827
                    (fun (con data) TxOut_2616)
                  )
                  (lam
                    d_2809
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2810
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2809 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2811 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2810
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2812 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_2811 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2821 [ Maybe_2597 (con bytestring) ])
                        [
                          [
                            {
                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2761
                              (con bytestring)
                            }
                            unsafeDataAsB_2697
                          ]
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2812 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_2816
                          [
                            [
                              (lam
                                k_2817
                                (type)
                                (lam
                                  v_2818
                                  (type)
                                  [
                                    List_2575 [ [ Tuple2_2582 k_2817 ] v_2818 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_2819
                                  (type)
                                  (lam
                                    v_2820
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_2819 ] v_2820 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                        )
                        [
                          [
                            [
                              {
                                {
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2736
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_2814
                                      (type)
                                      (lam
                                        v_2815
                                        (type)
                                        [
                                          List_2575
                                          [ [ Tuple2_2582 k_2814 ] v_2815 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2697
                            ]
                            fUnsafeFromDataValue_2743
                          ]
                          [ { head_2688 (con data) } t_2812 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2813 Address_2613)
                        [
                          fUnsafeFromDataAddress_cunsafeFromBuiltinData_2808
                          [ { head_2688 (con data) } t_2811 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2822 TxOut_2616)
                        [ [ [ TxOut_2617 x_2813 ] x_2816 ] x_2821 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) TxOut_2616) }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2810
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2823 (con unit) x_2822)
                          ]
                          (lam ds_2826 (con unit) x_2825)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2857
                    (fun (con data) TxInInfo_2627)
                  )
                  (lam
                    d_2847
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2848
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_2847 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2849 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2848
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2851 TxOut_2616)
                        [
                          fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2827
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_2849 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2850 TxOutRef_2624)
                        [
                          fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2846
                          [ { head_2688 (con data) } t_2849 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2852 TxInInfo_2627)
                        [ [ TxInInfo_2628 x_2850 ] x_2851 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) TxInInfo_2627) }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2848
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2853 (con unit) x_2852)
                          ]
                          (lam ds_2856 (con unit) x_2855)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    unsafeDataAsList_2698
                    (fun (con data) [ (con list) (con data) ])
                  )
                  (builtin unListData)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                    (all
                      a_2711
                      (type)
                      (fun
                        [ (lam a_2712 (type) (fun (con data) a_2712)) a_2711 ]
                        (fun (con data) [ List_2575 a_2711 ])
                      )
                    )
                  )
                  (abs
                    a_2702
                    (type)
                    (lam
                      dUnsafeFromData_2703
                      [ (lam a_2704 (type) (fun (con data) a_2704)) a_2702 ]
                      (let
                        (rec)
                        (termbind
                          (strict)
                          (vardecl
                            go_2706
                            (fun [ (con list) (con data) ] [ List_2575 a_2702 ])
                          )
                          (lam
                            l_2707
                            [ (con list) (con data) ]
                            [
                              [
                                [
                                  [
                                    {
                                      { chooseList_2699 (con data) }
                                      (fun Unit_2563 [ List_2575 a_2702 ])
                                    }
                                    l_2707
                                  ]
                                  (lam ds_2708 Unit_2563 { Nil_2576 a_2702 })
                                ]
                                (lam
                                  ds_2709
                                  Unit_2563
                                  [
                                    [
                                      { Cons_2577 a_2702 }
                                      [
                                        dUnsafeFromData_2703
                                        [ { head_2688 (con data) } l_2707 ]
                                      ]
                                    ]
                                    [
                                      go_2706
                                      [ { tail_2680 (con data) } l_2707 ]
                                    ]
                                  ]
                                )
                              ]
                              Unit_2564
                            ]
                          )
                        )
                        (lam
                          d_2705
                          (con data)
                          [ go_2706 [ unsafeDataAsList_2698 d_2705 ] ]
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3040
                    (fun (con data) TxInfo_2659)
                  )
                  (lam
                    d_3002
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3003
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_3002 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3004 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3003
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3005 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3004 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3006 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3005 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3007 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3006 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3008 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3007 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3009 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3008 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3010 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3009 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3011 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3010 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3012 [ (con list) (con data) ])
                        [ { tail_2680 (con data) } t_3011 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3034 (con bytestring))
                        [
                          fUnsafeFromDataTxId_cunsafeFromBuiltinData_2835
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_3012 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3033
                          [
                            List_2575
                            [ [ Tuple2_2582 (con bytestring) ] (con data) ]
                          ]
                        )
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              [ [ Tuple2_2582 (con bytestring) ] (con data) ]
                            }
                            [
                              [
                                {
                                  {
                                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2997
                                    (con bytestring)
                                  }
                                  (con data)
                                }
                                unsafeDataAsB_2697
                              ]
                              fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_2980
                            ]
                          ]
                          [ { head_2688 (con data) } t_3012 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3032 [ List_2575 (con bytestring) ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              (con bytestring)
                            }
                            unsafeDataAsB_2697
                          ]
                          [ { head_2688 (con data) } t_3011 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3031 [ Interval_2645 (con integer) ])
                        [
                          [
                            {
                              fUnsafeFromDataInterval_cunsafeFromBuiltinData_2934
                              (con integer)
                            }
                            unsafeDataAsI_2696
                          ]
                          [ { head_2688 (con data) } t_3010 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3030
                          [
                            List_2575
                            [
                              [ Tuple2_2582 StakingCredential_2609 ]
                              (con integer)
                            ]
                          ]
                        )
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              [
                                [ Tuple2_2582 StakingCredential_2609 ]
                                (con integer)
                              ]
                            }
                            [
                              [
                                {
                                  {
                                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2997
                                    StakingCredential_2609
                                  }
                                  (con integer)
                                }
                                fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2797
                              ]
                              unsafeDataAsI_2696
                            ]
                          ]
                          [ { head_2688 (con data) } t_3009 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3029 [ List_2575 DCert_2650 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              DCert_2650
                            }
                            fUnsafeFromDataDCert_cunsafeFromBuiltinData_2978
                          ]
                          [ { head_2688 (con data) } t_3008 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3024
                          [
                            [
                              (lam
                                k_3025
                                (type)
                                (lam
                                  v_3026
                                  (type)
                                  [
                                    List_2575 [ [ Tuple2_2582 k_3025 ] v_3026 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_3027
                                  (type)
                                  (lam
                                    v_3028
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_3027 ] v_3028 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                        )
                        [
                          [
                            [
                              {
                                {
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2736
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_3022
                                      (type)
                                      (lam
                                        v_3023
                                        (type)
                                        [
                                          List_2575
                                          [ [ Tuple2_2582 k_3022 ] v_3023 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2697
                            ]
                            fUnsafeFromDataValue_2743
                          ]
                          [ { head_2688 (con data) } t_3007 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3017
                          [
                            [
                              (lam
                                k_3018
                                (type)
                                (lam
                                  v_3019
                                  (type)
                                  [
                                    List_2575 [ [ Tuple2_2582 k_3018 ] v_3019 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_3020
                                  (type)
                                  (lam
                                    v_3021
                                    (type)
                                    [
                                      List_2575
                                      [ [ Tuple2_2582 k_3020 ] v_3021 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                        )
                        [
                          [
                            [
                              {
                                {
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2736
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_3015
                                      (type)
                                      (lam
                                        v_3016
                                        (type)
                                        [
                                          List_2575
                                          [ [ Tuple2_2582 k_3015 ] v_3016 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2697
                            ]
                            fUnsafeFromDataValue_2743
                          ]
                          [ { head_2688 (con data) } t_3006 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3014 [ List_2575 TxOut_2616 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              TxOut_2616
                            }
                            fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2827
                          ]
                          [ { head_2688 (con data) } t_3005 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3013 [ List_2575 TxInInfo_2627 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2710
                              TxInInfo_2627
                            }
                            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2857
                          ]
                          [ { head_2688 (con data) } t_3004 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3035 TxInfo_2659)
                        [
                          [
                            [
                              [
                                [
                                  [
                                    [
                                      [
                                        [ [ TxInfo_2660 x_3013 ] x_3014 ] x_3017
                                      ]
                                      x_3024
                                    ]
                                    x_3029
                                  ]
                                  x_3030
                                ]
                                x_3031
                              ]
                              x_3032
                            ]
                            x_3033
                          ]
                          x_3034
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2686 (fun (con unit) TxInfo_2659) }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3003
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3036 (con unit) x_3035)
                          ]
                          (lam ds_3039 (con unit) x_3038)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3076
                    (fun (con data) ScriptContext_2676)
                  )
                  (lam
                    d_3066
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3067
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2679 d_3066 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3068 [ (con list) (con data) ])
                        [
                          {
                            { snd_2682 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3067
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3070 ScriptPurpose_2670)
                        [
                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3065
                          [
                            { head_2688 (con data) }
                            [ { tail_2680 (con data) } t_3068 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3069 TxInfo_2659)
                        [
                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3040
                          [ { head_2688 (con data) } t_3068 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3071 ScriptContext_2676)
                        [ [ ScriptContext_2677 x_3069 ] x_3070 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2686
                                (fun (con unit) ScriptContext_2676)
                              }
                              [
                                [
                                  equalsInteger_2693
                                  [
                                    {
                                      { fst_2690 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3067
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3072 (con unit) x_3071)
                          ]
                          (lam ds_3075 (con unit) x_3074)
                        ]
                        unitval_2554
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl checkHasFailedError_2571 (con string))
                  (con string "PT5")
                )
                (termbind
                  (strict)
                  (vardecl
                    traceError_2569
                    (all a_2570 (type) (fun (con string) a_2570))
                  )
                  (abs
                    a_2566
                    (type)
                    (lam
                      str_2567
                      (con string)
                      [
                        { error_2559 a_2566 }
                        [
                          {
                            [
                              Unit_match_2565
                              [
                                [ { trace_2555 Unit_2563 } str_2567 ] Unit_2564
                              ]
                            ]
                            (con unit)
                          }
                          unitval_2554
                        ]
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    wrapMintingPolicy_3088
                    (all
                      r_3089
                      (type)
                      (fun
                        [ (lam a_3090 (type) (fun (con data) a_3090)) r_3089 ]
                        (fun
                          (fun r_3089 (fun ScriptContext_2676 Bool_2527))
                          (fun (con data) (fun (con data) Unit_2563))
                        )
                      )
                    )
                  )
                  (abs
                    r_3077
                    (type)
                    (lam
                      dUnsafeFromData_3078
                      [ (lam a_3079 (type) (fun (con data) a_3079)) r_3077 ]
                      (lam
                        f_3080
                        (fun r_3077 (fun ScriptContext_2676 Bool_2527))
                        (lam
                          r_3081
                          (con data)
                          (lam
                            p_3082
                            (con data)
                            {
                              [
                                [
                                  {
                                    [
                                      Bool_match_2530
                                      [
                                        [
                                          f_3080 [ dUnsafeFromData_3078 r_3081 ]
                                        ]
                                        [
                                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3076
                                          p_3082
                                        ]
                                      ]
                                    ]
                                    (all dead_3084 (type) Unit_2563)
                                  }
                                  (abs dead_3085 (type) Unit_2564)
                                ]
                                (abs
                                  dead_3086
                                  (type)
                                  [
                                    { traceError_2569 Unit_2563 }
                                    checkHasFailedError_2571
                                  ]
                                )
                              ]
                              (all dead_3087 (type) dead_3087)
                            }
                          )
                        )
                      )
                    )
                  )
                )
                (lam
                  oref_3442
                  TxOutRef_2624
                  (lam
                    tn_3443
                    (con bytestring)
                    [
                      [
                        {
                          {
                            bad_name_3439
                            (fun Unit_2563 (fun ScriptContext_2676 Bool_2527))
                          }
                          (fun (con data) (fun (con data) Unit_2563))
                        }
                        [
                          { wrapMintingPolicy_3088 Unit_2563 }
                          fUnsafeFromDataUnit_3433
                        ]
                      ]
                      [ [ mkPolicy_3392 oref_3442 ] tn_3443 ]
                    ]
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)