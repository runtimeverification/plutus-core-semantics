(program
  (let
    (nonrec)
    (termbind
      (strict)
      (vardecl
        bad_name_3743
        (all
          a_3744
          (type)
          (all b_3745 (type) (fun (fun a_3744 b_3745) (fun a_3744 b_3745)))
        )
      )
      (abs
        a_3739
        (type)
        (abs
          b_3740
          (type)
          (lam f_3741 (fun a_3739 b_3740) (lam a_3742 a_3739 [ f_3741 a_3742 ]))
        )
      )
    )
    (termbind
      (strict)
      (vardecl reconstructCaseError_2852 (con string))
      (con string "PT1")
    )
    (datatypebind
      (datatype
        (tyvardecl Unit_2730 (type))

        Unit_match_2732
        (vardecl Unit_2731 Unit_2730)
      )
    )
    (termbind
      (strict)
      (vardecl error_2726 (all a_2727 (type) (fun (con unit) a_2727)))
      (abs a_2724 (type) (lam thunk_2725 (con unit) (error a_2724)))
    )
    (termbind
      (strict)
      (vardecl
        trace_2722 (all a_2723 (type) (fun (con string) (fun a_2723 a_2723)))
      )
      (builtin trace)
    )
    (termbind (strict) (vardecl unitval_2721 (con unit)) (con unit ()))
    (termbind
      (nonstrict)
      (vardecl x_3734 Unit_2730)
      [
        { error_2726 Unit_2730 }
        [
          {
            [
              Unit_match_2732
              [
                [ { trace_2722 Unit_2730 } reconstructCaseError_2852 ] Unit_2731
              ]
            ]
            (con unit)
          }
          unitval_2721
        ]
      ]
    )
    (termbind
      (strict)
      (vardecl
        equalsInteger_2860 (fun (con integer) (fun (con integer) (con bool)))
      )
      (builtin equalsInteger)
    )
    (termbind
      (strict)
      (vardecl
        fst_2857
        (all
          a_2858
          (type)
          (all b_2859 (type) (fun [ [ (con pair) a_2858 ] b_2859 ] a_2858))
        )
      )
      (builtin fstPair)
    )
    (termbind
      (strict)
      (vardecl
        ifThenElse_2853
        (all a_2854 (type) (fun (con bool) (fun a_2854 (fun a_2854 a_2854))))
      )
      (builtin ifThenElse)
    )
    (termbind
      (strict)
      (vardecl
        unsafeDataAsConstr_2846
        (fun
          (con data) [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
        )
      )
      (builtin unConstrData)
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataUnit_cunsafeFromBuiltinData_3736
        (fun (con data) Unit_2730)
      )
      (lam
        d_3731
        (con data)
        [
          [
            [
              [
                { ifThenElse_2853 (fun (con unit) Unit_2730) }
                [
                  [
                    equalsInteger_2860
                    [
                      { { fst_2857 (con integer) } [ (con list) (con data) ] }
                      [ unsafeDataAsConstr_2846 d_3731 ]
                    ]
                  ]
                  (con integer 0)
                ]
              ]
              (lam ds_3732 (con unit) Unit_2731)
            ]
            (lam ds_3735 (con unit) x_3734)
          ]
          unitval_2721
        ]
      )
    )
    (termbind
      (nonstrict)
      (vardecl
        fUnsafeFromDataUnit_3737
        [ (lam a_3738 (type) (fun (con data) a_3738)) Unit_2730 ]
      )
      fUnsafeFromDataUnit_cunsafeFromBuiltinData_3736
    )
    (datatypebind
      (datatype
        (tyvardecl AdditiveMonoid_2708 (fun (type) (type)))
        (tyvardecl a_2711 (type))
        AdditiveMonoid_match_2710
        (vardecl
          CConsAdditiveMonoid_2709
          (fun
            [ (lam a_2712 (type) (fun a_2712 (fun a_2712 a_2712))) a_2711 ]
            (fun a_2711 [ AdditiveMonoid_2708 a_2711 ])
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool_2694 (type))

        Bool_match_2697
        (vardecl True_2695 Bool_2694) (vardecl False_2696 Bool_2694)
      )
    )
    (termbind
      (strict)
      (vardecl bad_name_2705 (fun Bool_2694 (fun Bool_2694 Bool_2694)))
      (lam
        l_2698
        Bool_2694
        (lam
          r_2699
          Bool_2694
          {
            [
              [
                { [ Bool_match_2697 l_2698 ] (all dead_2701 (type) Bool_2694) }
                (abs dead_2702 (type) True_2695)
              ]
              (abs dead_2703 (type) r_2699)
            ]
            (all dead_2704 (type) dead_2704)
          }
        )
      )
    )
    (termbind
      (nonstrict)
      (vardecl fAdditiveMonoidBool_3430 [ AdditiveMonoid_2708 Bool_2694 ])
      [ [ { CConsAdditiveMonoid_2709 Bool_2694 } bad_name_2705 ] False_2696 ]
    )
    (termbind
      (strict)
      (vardecl
        p1AdditiveMonoid_3356
        (all
          a_3357
          (type)
          (fun
            [ AdditiveMonoid_2708 a_3357 ]
            [ (lam a_3358 (type) (fun a_3358 (fun a_3358 a_3358))) a_3357 ]
          )
        )
      )
      (abs
        a_3349
        (type)
        (lam
          v_3350
          [ AdditiveMonoid_2708 a_3349 ]
          [
            {
              [ { AdditiveMonoid_match_2710 a_3349 } v_3350 ]
              [ (lam a_3352 (type) (fun a_3352 (fun a_3352 a_3352))) a_3349 ]
            }
            (lam
              v_3353
              [ (lam a_3354 (type) (fun a_3354 (fun a_3354 a_3354))) a_3349 ]
              (lam v_3355 a_3349 v_3353)
            )
          ]
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Monoid_3344 (fun (type) (type)))
        (tyvardecl a_3347 (type))
        Monoid_match_3346
        (vardecl
          CConsMonoid_3345
          (fun
            [ (lam a_3348 (type) (fun a_3348 (fun a_3348 a_3348))) a_3347 ]
            (fun a_3347 [ Monoid_3344 a_3347 ])
          )
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        zero_2719
        (all a_2720 (type) (fun [ AdditiveMonoid_2708 a_2720 ] a_2720))
      )
      (abs
        a_2713
        (type)
        (lam
          v_2714
          [ AdditiveMonoid_2708 a_2713 ]
          [
            { [ { AdditiveMonoid_match_2710 a_2713 } v_2714 ] a_2713 }
            (lam
              v_2716
              [ (lam a_2717 (type) (fun a_2717 (fun a_2717 a_2717))) a_2713 ]
              (lam v_2718 a_2713 v_2718)
            )
          ]
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fMonoidSum_3366
        (all
          a_3367
          (type)
          (fun
            [ AdditiveMonoid_2708 a_3367 ]
            [ Monoid_3344 [ (lam a_3368 (type) a_3368) a_3367 ] ]
          )
        )
      )
      (abs
        a_3359
        (type)
        (lam
          v_3360
          [ AdditiveMonoid_2708 a_3359 ]
          [
            [
              { CConsMonoid_3345 [ (lam a_3361 (type) a_3361) a_3359 ] }
              (lam
                eta_3362
                [ (lam a_3363 (type) a_3363) a_3359 ]
                (lam
                  eta_3364
                  [ (lam a_3365 (type) a_3365) a_3359 ]
                  [
                    [ [ { p1AdditiveMonoid_3356 a_3359 } v_3360 ] eta_3362 ]
                    eta_3364
                  ]
                )
              )
            ]
            [ { zero_2719 a_3359 } v_3360 ]
          ]
        )
      )
    )
    (termbind
      (nonstrict)
      (vardecl
        dMonoid_3440 [ Monoid_3344 [ (lam a_3441 (type) a_3441) Bool_2694 ] ]
      )
      [ { fMonoidSum_3366 Bool_2694 } fAdditiveMonoidBool_3430 ]
    )
    (termbind
      (strict)
      (vardecl
        equalsByteString_3336
        (fun (con bytestring) (fun (con bytestring) (con bool)))
      )
      (builtin equalsByteString)
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef_2791 (type))

        TxOutRef_match_2793
        (vardecl
          TxOutRef_2792 (fun (con bytestring) (fun (con integer) TxOutRef_2791))
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        fEqTxOutRef_c_3429 (fun TxOutRef_2791 (fun TxOutRef_2791 Bool_2694))
      )
      (lam
        l_3404
        TxOutRef_2791
        (lam
          r_3405
          TxOutRef_2791
          {
            [
              [
                {
                  [
                    Bool_match_2697
                    [
                      [
                        [
                          { ifThenElse_2853 Bool_2694 }
                          [
                            [
                              equalsByteString_3336
                              [
                                {
                                  [ TxOutRef_match_2793 l_3404 ]
                                  (con bytestring)
                                }
                                (lam
                                  ds_3414
                                  (con bytestring)
                                  (lam ds_3415 (con integer) ds_3414)
                                )
                              ]
                            ]
                            [
                              {
                                [ TxOutRef_match_2793 r_3405 ] (con bytestring)
                              }
                              (lam
                                ds_3417
                                (con bytestring)
                                (lam ds_3418 (con integer) ds_3417)
                              )
                            ]
                          ]
                        ]
                        True_2695
                      ]
                      False_2696
                    ]
                  ]
                  (all dead_3419 (type) Bool_2694)
                }
                (abs
                  dead_3420
                  (type)
                  [
                    [
                      [
                        { ifThenElse_2853 Bool_2694 }
                        [
                          [
                            equalsInteger_2860
                            [
                              { [ TxOutRef_match_2793 l_3404 ] (con integer) }
                              (lam
                                ds_3422
                                (con bytestring)
                                (lam ds_3423 (con integer) ds_3423)
                              )
                            ]
                          ]
                          [
                            { [ TxOutRef_match_2793 r_3405 ] (con integer) }
                            (lam
                              ds_3425
                              (con bytestring)
                              (lam ds_3426 (con integer) ds_3426)
                            )
                          ]
                        ]
                      ]
                      True_2695
                    ]
                    False_2696
                  ]
                )
              ]
              (abs dead_3427 (type) False_2696)
            ]
            (all dead_3428 (type) dead_3428)
          }
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        p1Monoid_3384
        (all
          a_3385
          (type)
          (fun
            [ Monoid_3344 a_3385 ]
            [ (lam a_3386 (type) (fun a_3386 (fun a_3386 a_3386))) a_3385 ]
          )
        )
      )
      (abs
        a_3377
        (type)
        (lam
          v_3378
          [ Monoid_3344 a_3377 ]
          [
            {
              [ { Monoid_match_3346 a_3377 } v_3378 ]
              [ (lam a_3380 (type) (fun a_3380 (fun a_3380 a_3380))) a_3377 ]
            }
            (lam
              v_3381
              [ (lam a_3382 (type) (fun a_3382 (fun a_3382 a_3382))) a_3377 ]
              (lam v_3383 a_3377 v_3381)
            )
          ]
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        mempty_3375 (all a_3376 (type) (fun [ Monoid_3344 a_3376 ] a_3376))
      )
      (abs
        a_3369
        (type)
        (lam
          v_3370
          [ Monoid_3344 a_3369 ]
          [
            { [ { Monoid_match_3346 a_3369 } v_3370 ] a_3369 }
            (lam
              v_3372
              [ (lam a_3373 (type) (fun a_3373 (fun a_3373 a_3373))) a_3369 ]
              (lam v_3374 a_3369 v_3374)
            )
          ]
        )
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List_2742 (fun (type) (type)))
          (tyvardecl a_2746 (type))
          Nil_match_2745
          (vardecl Nil_2743 [ List_2742 a_2746 ])
          (vardecl
            Cons_2744
            (fun a_2746 (fun [ List_2742 a_2746 ] [ List_2742 a_2746 ]))
          )
        )
      )
      (let
        (rec)
        (termbind
          (strict)
          (vardecl
            fFoldableNil_cfoldMap_3387
            (all
              m_3388
              (type)
              (all
                a_3389
                (type)
                (fun
                  [ Monoid_3344 m_3388 ]
                  (fun (fun a_3389 m_3388) (fun [ List_2742 a_3389 ] m_3388))
                )
              )
            )
          )
          (abs
            m_3390
            (type)
            (abs
              a_3391
              (type)
              (lam
                dMonoid_3392
                [ Monoid_3344 m_3390 ]
                (let
                  (nonrec)
                  (termbind
                    (nonstrict)
                    (vardecl
                      dSemigroup_3393
                      [
                        (lam a_3394 (type) (fun a_3394 (fun a_3394 a_3394)))
                        m_3390
                      ]
                    )
                    [ { p1Monoid_3384 m_3390 } dMonoid_3392 ]
                  )
                  (lam
                    ds_3395
                    (fun a_3391 m_3390)
                    (lam
                      ds_3396
                      [ List_2742 a_3391 ]
                      {
                        [
                          [
                            {
                              [ { Nil_match_2745 a_3391 } ds_3396 ]
                              (all dead_3398 (type) m_3390)
                            }
                            (abs
                              dead_3399
                              (type)
                              [ { mempty_3375 m_3390 } dMonoid_3392 ]
                            )
                          ]
                          (lam
                            x_3400
                            a_3391
                            (lam
                              xs_3401
                              [ List_2742 a_3391 ]
                              (abs
                                dead_3402
                                (type)
                                [
                                  [ dSemigroup_3393 [ ds_3395 x_3400 ] ]
                                  [
                                    [
                                      [
                                        {
                                          { fFoldableNil_cfoldMap_3387 m_3390 }
                                          a_3391
                                        }
                                        dMonoid_3392
                                      ]
                                      ds_3395
                                    ]
                                    xs_3401
                                  ]
                                ]
                              )
                            )
                          )
                        ]
                        (all dead_3403 (type) dead_3403)
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
              equalsByteString_3339
              (fun (con bytestring) (fun (con bytestring) Bool_2694))
            )
            (lam
              x_3337
              (con bytestring)
              (lam
                y_3338
                (con bytestring)
                [
                  [
                    [
                      { ifThenElse_2853 Bool_2694 }
                      [ [ equalsByteString_3336 x_3337 ] y_3338 ]
                    ]
                    True_2695
                  ]
                  False_2696
                ]
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl
                Tuple3_3270 (fun (type) (fun (type) (fun (type) (type))))
              )
              (tyvardecl a_3273 (type))
              (tyvardecl b_3274 (type))
              (tyvardecl c_3275 (type))
              Tuple3_match_3272
              (vardecl
                Tuple3_3271
                (fun
                  a_3273
                  (fun
                    b_3274
                    (fun c_3275 [ [ [ Tuple3_3270 a_3273 ] b_3274 ] c_3275 ])
                  )
                )
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Tuple2_2749 (fun (type) (fun (type) (type))))
              (tyvardecl a_2752 (type)) (tyvardecl b_2753 (type))
              Tuple2_match_2751
              (vardecl
                Tuple2_2750
                (fun a_2752 (fun b_2753 [ [ Tuple2_2749 a_2752 ] b_2753 ]))
              )
            )
          )
          (let
            (rec)
            (termbind
              (strict)
              (vardecl
                goInner_3311
                (fun
                  (con bytestring)
                  (fun
                    [
                      List_2742
                      [
                        [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        List_2742
                        [ [ Tuple2_2749 (con bytestring) ] (con integer) ]
                      ]
                      [
                        List_2742
                        [
                          [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                )
              )
              (lam
                ds_3312
                (con bytestring)
                (lam
                  acc_3313
                  [
                    List_2742
                    [
                      [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                      (con integer)
                    ]
                  ]
                  (lam
                    ds_3314
                    [
                      List_2742
                      [ [ Tuple2_2749 (con bytestring) ] (con integer) ]
                    ]
                    {
                      [
                        [
                          {
                            [
                              {
                                Nil_match_2745
                                [
                                  [ Tuple2_2749 (con bytestring) ] (con integer)
                                ]
                              }
                              ds_3314
                            ]
                            (all
                              dead_3316
                              (type)
                              [
                                List_2742
                                [
                                  [
                                    [ Tuple3_3270 (con bytestring) ]
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                            )
                          }
                          (abs dead_3317 (type) acc_3313)
                        ]
                        (lam
                          ipv_3318
                          [ [ Tuple2_2749 (con bytestring) ] (con integer) ]
                          (lam
                            ipv_3319
                            [
                              List_2742
                              [ [ Tuple2_2749 (con bytestring) ] (con integer) ]
                            ]
                            (abs
                              dead_3320
                              (type)
                              [
                                {
                                  [
                                    {
                                      { Tuple2_match_2751 (con bytestring) }
                                      (con integer)
                                    }
                                    ipv_3318
                                  ]
                                  [
                                    List_2742
                                    [
                                      [
                                        [ Tuple3_3270 (con bytestring) ]
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                  ]
                                }
                                (lam
                                  tn_3322
                                  (con bytestring)
                                  (lam
                                    a_3323
                                    (con integer)
                                    {
                                      [
                                        [
                                          {
                                            [
                                              Bool_match_2697
                                              [
                                                [
                                                  [
                                                    {
                                                      ifThenElse_2853 Bool_2694
                                                    }
                                                    [
                                                      [
                                                        equalsInteger_2860
                                                        a_3323
                                                      ]
                                                      (con integer 0)
                                                    ]
                                                  ]
                                                  True_2695
                                                ]
                                                False_2696
                                              ]
                                            ]
                                            (all
                                              dead_3325
                                              (type)
                                              [
                                                List_2742
                                                [
                                                  [
                                                    [
                                                      Tuple3_3270
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
                                            dead_3326
                                            (type)
                                            [
                                              [
                                                [ goInner_3311 ds_3312 ]
                                                acc_3313
                                              ]
                                              ipv_3319
                                            ]
                                          )
                                        ]
                                        (abs
                                          dead_3327
                                          (type)
                                          [
                                            [
                                              [ goInner_3311 ds_3312 ]
                                              [
                                                [
                                                  {
                                                    Cons_2744
                                                    [
                                                      [
                                                        [
                                                          Tuple3_3270
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
                                                              Tuple3_3271
                                                              (con bytestring)
                                                            }
                                                            (con bytestring)
                                                          }
                                                          (con integer)
                                                        }
                                                        ds_3312
                                                      ]
                                                      tn_3322
                                                    ]
                                                    a_3323
                                                  ]
                                                ]
                                                acc_3313
                                              ]
                                            ]
                                            ipv_3319
                                          ]
                                        )
                                      ]
                                      (all dead_3328 (type) dead_3328)
                                    }
                                  )
                                )
                              ]
                            )
                          )
                        )
                      ]
                      (all dead_3329 (type) dead_3329)
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
                  goOuter_3281
                  (fun
                    [
                      List_2742
                      [
                        [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        List_2742
                        [
                          [ Tuple2_2749 (con bytestring) ]
                          [
                            [
                              (lam
                                k_3282
                                (type)
                                (lam
                                  v_3283
                                  (type)
                                  [
                                    List_2742 [ [ Tuple2_2749 k_3282 ] v_3283 ]
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
                        List_2742
                        [
                          [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                )
                (lam
                  acc_3284
                  [
                    List_2742
                    [
                      [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                      (con integer)
                    ]
                  ]
                  (lam
                    ds_3285
                    [
                      List_2742
                      [
                        [ Tuple2_2749 (con bytestring) ]
                        [
                          [
                            (lam
                              k_3286
                              (type)
                              (lam
                                v_3287
                                (type)
                                [ List_2742 [ [ Tuple2_2749 k_3286 ] v_3287 ] ]
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
                                Nil_match_2745
                                [
                                  [ Tuple2_2749 (con bytestring) ]
                                  [
                                    [
                                      (lam
                                        k_3291
                                        (type)
                                        (lam
                                          v_3292
                                          (type)
                                          [
                                            List_2742
                                            [ [ Tuple2_2749 k_3291 ] v_3292 ]
                                          ]
                                        )
                                      )
                                      (con bytestring)
                                    ]
                                    (con integer)
                                  ]
                                ]
                              }
                              ds_3285
                            ]
                            (all
                              dead_3293
                              (type)
                              [
                                List_2742
                                [
                                  [
                                    [ Tuple3_3270 (con bytestring) ]
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                            )
                          }
                          (abs dead_3294 (type) acc_3284)
                        ]
                        (lam
                          ipv_3295
                          [
                            [ Tuple2_2749 (con bytestring) ]
                            [
                              [
                                (lam
                                  k_3296
                                  (type)
                                  (lam
                                    v_3297
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_3296 ] v_3297 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                          (lam
                            ipv_3298
                            [
                              List_2742
                              [
                                [ Tuple2_2749 (con bytestring) ]
                                [
                                  [
                                    (lam
                                      k_3299
                                      (type)
                                      (lam
                                        v_3300
                                        (type)
                                        [
                                          List_2742
                                          [ [ Tuple2_2749 k_3299 ] v_3300 ]
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
                              dead_3301
                              (type)
                              [
                                {
                                  [
                                    {
                                      { Tuple2_match_2751 (con bytestring) }
                                      [
                                        [
                                          (lam
                                            k_3305
                                            (type)
                                            (lam
                                              v_3306
                                              (type)
                                              [
                                                List_2742
                                                [
                                                  [ Tuple2_2749 k_3305 ] v_3306
                                                ]
                                              ]
                                            )
                                          )
                                          (con bytestring)
                                        ]
                                        (con integer)
                                      ]
                                    }
                                    ipv_3295
                                  ]
                                  [
                                    List_2742
                                    [
                                      [
                                        [ Tuple3_3270 (con bytestring) ]
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                  ]
                                }
                                (lam
                                  cs_3307
                                  (con bytestring)
                                  (lam
                                    m_3308
                                    [
                                      [
                                        (lam
                                          k_3309
                                          (type)
                                          (lam
                                            v_3310
                                            (type)
                                            [
                                              List_2742
                                              [ [ Tuple2_2749 k_3309 ] v_3310 ]
                                            ]
                                          )
                                        )
                                        (con bytestring)
                                      ]
                                      (con integer)
                                    ]
                                    [
                                      [
                                        goOuter_3281
                                        [
                                          [ [ goInner_3311 cs_3307 ] acc_3284 ]
                                          m_3308
                                        ]
                                      ]
                                      ipv_3298
                                    ]
                                  )
                                )
                              ]
                            )
                          )
                        )
                      ]
                      (all dead_3330 (type) dead_3330)
                    }
                  )
                )
              )
              (let
                (nonrec)
                (termbind
                  (strict)
                  (vardecl
                    flattenValue_3331
                    (fun
                      [
                        [
                          (lam
                            k_3332
                            (type)
                            (lam
                              v_3333
                              (type)
                              [ List_2742 [ [ Tuple2_2749 k_3332 ] v_3333 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        [
                          [
                            (lam
                              k_3334
                              (type)
                              (lam
                                v_3335
                                (type)
                                [ List_2742 [ [ Tuple2_2749 k_3334 ] v_3335 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                      [
                        List_2742
                        [
                          [ [ Tuple3_3270 (con bytestring) ] (con bytestring) ]
                          (con integer)
                        ]
                      ]
                    )
                  )
                  (lam
                    v_3276
                    [
                      [
                        (lam
                          k_3277
                          (type)
                          (lam
                            v_3278
                            (type)
                            [ List_2742 [ [ Tuple2_2749 k_3277 ] v_3278 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      [
                        [
                          (lam
                            k_3279
                            (type)
                            (lam
                              v_3280
                              (type)
                              [ List_2742 [ [ Tuple2_2749 k_3279 ] v_3280 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        (con integer)
                      ]
                    ]
                    [
                      [
                        goOuter_3281
                        {
                          Nil_2743
                          [
                            [
                              [ Tuple3_3270 (con bytestring) ] (con bytestring)
                            ]
                            (con integer)
                          ]
                        }
                      ]
                      v_3276
                    ]
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Credential_2772 (type))

                    Credential_match_2775
                    (vardecl
                      PubKeyCredential_2773
                      (fun (con bytestring) Credential_2772)
                    )
                    (vardecl
                      ScriptCredential_2774
                      (fun (con bytestring) Credential_2772)
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl StakingCredential_2776 (type))

                    StakingCredential_match_2779
                    (vardecl
                      StakingHash_2777
                      (fun Credential_2772 StakingCredential_2776)
                    )
                    (vardecl
                      StakingPtr_2778
                      (fun
                        (con integer)
                        (fun
                          (con integer)
                          (fun (con integer) StakingCredential_2776)
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl DCert_2817 (type))

                    DCert_match_2825
                    (vardecl
                      DCertDelegDeRegKey_2818
                      (fun StakingCredential_2776 DCert_2817)
                    )
                    (vardecl
                      DCertDelegDelegate_2819
                      (fun
                        StakingCredential_2776 (fun (con bytestring) DCert_2817)
                      )
                    )
                    (vardecl
                      DCertDelegRegKey_2820
                      (fun StakingCredential_2776 DCert_2817)
                    )
                    (vardecl DCertGenesis_2821 DCert_2817)
                    (vardecl DCertMir_2822 DCert_2817)
                    (vardecl
                      DCertPoolRegister_2823
                      (fun (con bytestring) (fun (con bytestring) DCert_2817))
                    )
                    (vardecl
                      DCertPoolRetire_2824
                      (fun (con bytestring) (fun (con integer) DCert_2817))
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl ScriptPurpose_2837 (type))

                    ScriptPurpose_match_2842
                    (vardecl
                      Certifying_2838 (fun DCert_2817 ScriptPurpose_2837)
                    )
                    (vardecl
                      Minting_2839 (fun (con bytestring) ScriptPurpose_2837)
                    )
                    (vardecl
                      Rewarding_2840
                      (fun StakingCredential_2776 ScriptPurpose_2837)
                    )
                    (vardecl
                      Spending_2841 (fun TxOutRef_2791 ScriptPurpose_2837)
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Extended_2798 (fun (type) (type)))
                    (tyvardecl a_2803 (type))
                    Extended_match_2802
                    (vardecl Finite_2799 (fun a_2803 [ Extended_2798 a_2803 ]))
                    (vardecl NegInf_2800 [ Extended_2798 a_2803 ])
                    (vardecl PosInf_2801 [ Extended_2798 a_2803 ])
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl LowerBound_2808 (fun (type) (type)))
                    (tyvardecl a_2811 (type))
                    LowerBound_match_2810
                    (vardecl
                      LowerBound_2809
                      (fun
                        [ Extended_2798 a_2811 ]
                        (fun Bool_2694 [ LowerBound_2808 a_2811 ])
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl UpperBound_2804 (fun (type) (type)))
                    (tyvardecl a_2807 (type))
                    UpperBound_match_2806
                    (vardecl
                      UpperBound_2805
                      (fun
                        [ Extended_2798 a_2807 ]
                        (fun Bool_2694 [ UpperBound_2804 a_2807 ])
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Interval_2812 (fun (type) (type)))
                    (tyvardecl a_2815 (type))
                    Interval_match_2814
                    (vardecl
                      Interval_2813
                      (fun
                        [ LowerBound_2808 a_2815 ]
                        (fun
                          [ UpperBound_2804 a_2815 ] [ Interval_2812 a_2815 ]
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Maybe_2764 (fun (type) (type)))
                    (tyvardecl a_2768 (type))
                    Maybe_match_2767
                    (vardecl Just_2765 (fun a_2768 [ Maybe_2764 a_2768 ]))
                    (vardecl Nothing_2766 [ Maybe_2764 a_2768 ])
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl Address_2780 (type))

                    Address_match_2782
                    (vardecl
                      Address_2781
                      (fun
                        Credential_2772
                        (fun [ Maybe_2764 StakingCredential_2776 ] Address_2780)
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxOut_2783 (type))

                    TxOut_match_2785
                    (vardecl
                      TxOut_2784
                      (fun
                        Address_2780
                        (fun
                          [
                            [
                              (lam
                                k_2786
                                (type)
                                (lam
                                  v_2787
                                  (type)
                                  [
                                    List_2742 [ [ Tuple2_2749 k_2786 ] v_2787 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_2788
                                  (type)
                                  (lam
                                    v_2789
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_2788 ] v_2789 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              (con integer)
                            ]
                          ]
                          (fun [ Maybe_2764 (con bytestring) ] TxOut_2783)
                        )
                      )
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxInInfo_2794 (type))

                    TxInInfo_match_2796
                    (vardecl
                      TxInInfo_2795
                      (fun TxOutRef_2791 (fun TxOut_2783 TxInInfo_2794))
                    )
                  )
                )
                (datatypebind
                  (datatype
                    (tyvardecl TxInfo_2826 (type))

                    TxInfo_match_2828
                    (vardecl
                      TxInfo_2827
                      (fun
                        [ List_2742 TxInInfo_2794 ]
                        (fun
                          [ List_2742 TxOut_2783 ]
                          (fun
                            [
                              [
                                (lam
                                  k_2829
                                  (type)
                                  (lam
                                    v_2830
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_2829 ] v_2830 ]
                                    ]
                                  )
                                )
                                (con bytestring)
                              ]
                              [
                                [
                                  (lam
                                    k_2831
                                    (type)
                                    (lam
                                      v_2832
                                      (type)
                                      [
                                        List_2742
                                        [ [ Tuple2_2749 k_2831 ] v_2832 ]
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
                                    k_2833
                                    (type)
                                    (lam
                                      v_2834
                                      (type)
                                      [
                                        List_2742
                                        [ [ Tuple2_2749 k_2833 ] v_2834 ]
                                      ]
                                    )
                                  )
                                  (con bytestring)
                                ]
                                [
                                  [
                                    (lam
                                      k_2835
                                      (type)
                                      (lam
                                        v_2836
                                        (type)
                                        [
                                          List_2742
                                          [ [ Tuple2_2749 k_2835 ] v_2836 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              ]
                              (fun
                                [ List_2742 DCert_2817 ]
                                (fun
                                  [
                                    List_2742
                                    [
                                      [ Tuple2_2749 StakingCredential_2776 ]
                                      (con integer)
                                    ]
                                  ]
                                  (fun
                                    [ Interval_2812 (con integer) ]
                                    (fun
                                      [ List_2742 (con bytestring) ]
                                      (fun
                                        [
                                          List_2742
                                          [
                                            [ Tuple2_2749 (con bytestring) ]
                                            (con data)
                                          ]
                                        ]
                                        (fun (con bytestring) TxInfo_2826)
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
                    (tyvardecl ScriptContext_2843 (type))

                    ScriptContext_match_2845
                    (vardecl
                      ScriptContext_2844
                      (fun
                        TxInfo_2826 (fun ScriptPurpose_2837 ScriptContext_2843)
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    mkPolicy_3696
                    (fun
                      TxOutRef_2791
                      (fun
                        (con bytestring)
                        (fun Unit_2730 (fun ScriptContext_2843 Bool_2694))
                      )
                    )
                  )
                  (lam
                    oref_3431
                    TxOutRef_2791
                    (lam
                      tn_3432
                      (con bytestring)
                      (lam
                        ds_3433
                        Unit_2730
                        (lam
                          ctx_3434
                          ScriptContext_2843
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl info_3439 TxInfo_2826)
                              [
                                {
                                  [ ScriptContext_match_2845 ctx_3434 ]
                                  TxInfo_2826
                                }
                                (lam
                                  ds_3437
                                  TxInfo_2826
                                  (lam ds_3438 ScriptPurpose_2837 ds_3437)
                                )
                              ]
                            )
                            [
                              { [ Unit_match_2732 ds_3433 ] Bool_2694 }
                              {
                                [
                                  [
                                    {
                                      [
                                        Bool_match_2697
                                        {
                                          [
                                            [
                                              {
                                                [
                                                  Bool_match_2697
                                                  [
                                                    [
                                                      [
                                                        {
                                                          {
                                                            fFoldableNil_cfoldMap_3387
                                                            [
                                                              (lam
                                                                a_3521
                                                                (type)
                                                                a_3521
                                                              )
                                                              Bool_2694
                                                            ]
                                                          }
                                                          TxInInfo_2794
                                                        }
                                                        dMonoid_3440
                                                      ]
                                                      (lam
                                                        i_3522
                                                        TxInInfo_2794
                                                        [
                                                          [
                                                            fEqTxOutRef_c_3429
                                                            [
                                                              {
                                                                [
                                                                  TxInInfo_match_2796
                                                                  i_3522
                                                                ]
                                                                TxOutRef_2791
                                                              }
                                                              (lam
                                                                ds_3524
                                                                TxOutRef_2791
                                                                (lam
                                                                  ds_3525
                                                                  TxOut_2783
                                                                  ds_3524
                                                                )
                                                              )
                                                            ]
                                                          ]
                                                          oref_3431
                                                        ]
                                                      )
                                                    ]
                                                    [
                                                      {
                                                        [
                                                          TxInfo_match_2828
                                                          info_3439
                                                        ]
                                                        [
                                                          List_2742
                                                          TxInInfo_2794
                                                        ]
                                                      }
                                                      (lam
                                                        ds_3527
                                                        [
                                                          List_2742
                                                          TxInInfo_2794
                                                        ]
                                                        (lam
                                                          ds_3528
                                                          [
                                                            List_2742 TxOut_2783
                                                          ]
                                                          (lam
                                                            ds_3529
                                                            [
                                                              [
                                                                (lam
                                                                  k_3530
                                                                  (type)
                                                                  (lam
                                                                    v_3531
                                                                    (type)
                                                                    [
                                                                      List_2742
                                                                      [
                                                                        [
                                                                          Tuple2_2749
                                                                          k_3530
                                                                        ]
                                                                        v_3531
                                                                      ]
                                                                    ]
                                                                  )
                                                                )
                                                                (con bytestring)
                                                              ]
                                                              [
                                                                [
                                                                  (lam
                                                                    k_3532
                                                                    (type)
                                                                    (lam
                                                                      v_3533
                                                                      (type)
                                                                      [
                                                                        List_2742
                                                                        [
                                                                          [
                                                                            Tuple2_2749
                                                                            k_3532
                                                                          ]
                                                                          v_3533
                                                                        ]
                                                                      ]
                                                                    )
                                                                  )
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                ]
                                                                (con integer)
                                                              ]
                                                            ]
                                                            (lam
                                                              ds_3534
                                                              [
                                                                [
                                                                  (lam
                                                                    k_3535
                                                                    (type)
                                                                    (lam
                                                                      v_3536
                                                                      (type)
                                                                      [
                                                                        List_2742
                                                                        [
                                                                          [
                                                                            Tuple2_2749
                                                                            k_3535
                                                                          ]
                                                                          v_3536
                                                                        ]
                                                                      ]
                                                                    )
                                                                  )
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                ]
                                                                [
                                                                  [
                                                                    (lam
                                                                      k_3537
                                                                      (type)
                                                                      (lam
                                                                        v_3538
                                                                        (type)
                                                                        [
                                                                          List_2742
                                                                          [
                                                                            [
                                                                              Tuple2_2749
                                                                              k_3537
                                                                            ]
                                                                            v_3538
                                                                          ]
                                                                        ]
                                                                      )
                                                                    )
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  ]
                                                                  (con integer)
                                                                ]
                                                              ]
                                                              (lam
                                                                ds_3539
                                                                [
                                                                  List_2742
                                                                  DCert_2817
                                                                ]
                                                                (lam
                                                                  ds_3540
                                                                  [
                                                                    List_2742
                                                                    [
                                                                      [
                                                                        Tuple2_2749
                                                                        StakingCredential_2776
                                                                      ]
                                                                      (con
                                                                        integer
                                                                      )
                                                                    ]
                                                                  ]
                                                                  (lam
                                                                    ds_3541
                                                                    [
                                                                      Interval_2812
                                                                      (con
                                                                        integer
                                                                      )
                                                                    ]
                                                                    (lam
                                                                      ds_3542
                                                                      [
                                                                        List_2742
                                                                        (con
                                                                          bytestring
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        ds_3543
                                                                        [
                                                                          List_2742
                                                                          [
                                                                            [
                                                                              Tuple2_2749
                                                                              (con
                                                                                bytestring
                                                                              )
                                                                            ]
                                                                            (con
                                                                              data
                                                                            )
                                                                          ]
                                                                        ]
                                                                        (lam
                                                                          ds_3544
                                                                          (con
                                                                            bytestring
                                                                          )
                                                                          ds_3527
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
                                                (all dead_3545 (type) Bool_2694)
                                              }
                                              (abs dead_3546 (type) True_2695)
                                            ]
                                            (abs
                                              dead_3547
                                              (type)
                                              [
                                                [
                                                  { trace_2722 Bool_2694 }
                                                  (con
                                                    string "UTxO not consumed"
                                                  )
                                                ]
                                                False_2696
                                              ]
                                            )
                                          ]
                                          (all dead_3548 (type) dead_3548)
                                        }
                                      ]
                                      (all dead_3549 (type) Bool_2694)
                                    }
                                    (abs
                                      dead_3550
                                      (type)
                                      {
                                        [
                                          [
                                            {
                                              [
                                                Bool_match_2697
                                                {
                                                  [
                                                    [
                                                      {
                                                        [
                                                          {
                                                            Nil_match_2745
                                                            [
                                                              [
                                                                [
                                                                  Tuple3_3270
                                                                  (con
                                                                    bytestring
                                                                  )
                                                                ]
                                                                (con bytestring)
                                                              ]
                                                              (con integer)
                                                            ]
                                                          }
                                                          [
                                                            flattenValue_3331
                                                            [
                                                              {
                                                                [
                                                                  TxInfo_match_2828
                                                                  info_3439
                                                                ]
                                                                [
                                                                  [
                                                                    (lam
                                                                      k_3646
                                                                      (type)
                                                                      (lam
                                                                        v_3647
                                                                        (type)
                                                                        [
                                                                          List_2742
                                                                          [
                                                                            [
                                                                              Tuple2_2749
                                                                              k_3646
                                                                            ]
                                                                            v_3647
                                                                          ]
                                                                        ]
                                                                      )
                                                                    )
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  ]
                                                                  [
                                                                    [
                                                                      (lam
                                                                        k_3648
                                                                        (type)
                                                                        (lam
                                                                          v_3649
                                                                          (type)
                                                                          [
                                                                            List_2742
                                                                            [
                                                                              [
                                                                                Tuple2_2749
                                                                                k_3648
                                                                              ]
                                                                              v_3649
                                                                            ]
                                                                          ]
                                                                        )
                                                                      )
                                                                      (con
                                                                        bytestring
                                                                      )
                                                                    ]
                                                                    (con
                                                                      integer
                                                                    )
                                                                  ]
                                                                ]
                                                              }
                                                              (lam
                                                                ds_3650
                                                                [
                                                                  List_2742
                                                                  TxInInfo_2794
                                                                ]
                                                                (lam
                                                                  ds_3651
                                                                  [
                                                                    List_2742
                                                                    TxOut_2783
                                                                  ]
                                                                  (lam
                                                                    ds_3652
                                                                    [
                                                                      [
                                                                        (lam
                                                                          k_3653
                                                                          (type)
                                                                          (lam
                                                                            v_3654
                                                                            (type)
                                                                            [
                                                                              List_2742
                                                                              [
                                                                                [
                                                                                  Tuple2_2749
                                                                                  k_3653
                                                                                ]
                                                                                v_3654
                                                                              ]
                                                                            ]
                                                                          )
                                                                        )
                                                                        (con
                                                                          bytestring
                                                                        )
                                                                      ]
                                                                      [
                                                                        [
                                                                          (lam
                                                                            k_3655
                                                                            (type)
                                                                            (lam
                                                                              v_3656
                                                                              (type)
                                                                              [
                                                                                List_2742
                                                                                [
                                                                                  [
                                                                                    Tuple2_2749
                                                                                    k_3655
                                                                                  ]
                                                                                  v_3656
                                                                                ]
                                                                              ]
                                                                            )
                                                                          )
                                                                          (con
                                                                            bytestring
                                                                          )
                                                                        ]
                                                                        (con
                                                                          integer
                                                                        )
                                                                      ]
                                                                    ]
                                                                    (lam
                                                                      ds_3657
                                                                      [
                                                                        [
                                                                          (lam
                                                                            k_3658
                                                                            (type)
                                                                            (lam
                                                                              v_3659
                                                                              (type)
                                                                              [
                                                                                List_2742
                                                                                [
                                                                                  [
                                                                                    Tuple2_2749
                                                                                    k_3658
                                                                                  ]
                                                                                  v_3659
                                                                                ]
                                                                              ]
                                                                            )
                                                                          )
                                                                          (con
                                                                            bytestring
                                                                          )
                                                                        ]
                                                                        [
                                                                          [
                                                                            (lam
                                                                              k_3660
                                                                              (type)
                                                                              (lam
                                                                                v_3661
                                                                                (type)
                                                                                [
                                                                                  List_2742
                                                                                  [
                                                                                    [
                                                                                      Tuple2_2749
                                                                                      k_3660
                                                                                    ]
                                                                                    v_3661
                                                                                  ]
                                                                                ]
                                                                              )
                                                                            )
                                                                            (con
                                                                              bytestring
                                                                            )
                                                                          ]
                                                                          (con
                                                                            integer
                                                                          )
                                                                        ]
                                                                      ]
                                                                      (lam
                                                                        ds_3662
                                                                        [
                                                                          List_2742
                                                                          DCert_2817
                                                                        ]
                                                                        (lam
                                                                          ds_3663
                                                                          [
                                                                            List_2742
                                                                            [
                                                                              [
                                                                                Tuple2_2749
                                                                                StakingCredential_2776
                                                                              ]
                                                                              (con
                                                                                integer
                                                                              )
                                                                            ]
                                                                          ]
                                                                          (lam
                                                                            ds_3664
                                                                            [
                                                                              Interval_2812
                                                                              (con
                                                                                integer
                                                                              )
                                                                            ]
                                                                            (lam
                                                                              ds_3665
                                                                              [
                                                                                List_2742
                                                                                (con
                                                                                  bytestring
                                                                                )
                                                                              ]
                                                                              (lam
                                                                                ds_3666
                                                                                [
                                                                                  List_2742
                                                                                  [
                                                                                    [
                                                                                      Tuple2_2749
                                                                                      (con
                                                                                        bytestring
                                                                                      )
                                                                                    ]
                                                                                    (con
                                                                                      data
                                                                                    )
                                                                                  ]
                                                                                ]
                                                                                (lam
                                                                                  ds_3667
                                                                                  (con
                                                                                    bytestring
                                                                                  )
                                                                                  ds_3657
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
                                                        (all
                                                          dead_3668
                                                          (type)
                                                          Bool_2694
                                                        )
                                                      }
                                                      (abs
                                                        dead_3669
                                                        (type)
                                                        False_2696
                                                      )
                                                    ]
                                                    (lam
                                                      ds_3670
                                                      [
                                                        [
                                                          [
                                                            Tuple3_3270
                                                            (con bytestring)
                                                          ]
                                                          (con bytestring)
                                                        ]
                                                        (con integer)
                                                      ]
                                                      (lam
                                                        ds_3671
                                                        [
                                                          List_2742
                                                          [
                                                            [
                                                              [
                                                                Tuple3_3270
                                                                (con bytestring)
                                                              ]
                                                              (con bytestring)
                                                            ]
                                                            (con integer)
                                                          ]
                                                        ]
                                                        (abs
                                                          dead_3672
                                                          (type)
                                                          [
                                                            {
                                                              [
                                                                {
                                                                  {
                                                                    {
                                                                      Tuple3_match_3272
                                                                      (con
                                                                        bytestring
                                                                      )
                                                                    }
                                                                    (con
                                                                      bytestring
                                                                    )
                                                                  }
                                                                  (con integer)
                                                                }
                                                                ds_3670
                                                              ]
                                                              Bool_2694
                                                            }
                                                            (lam
                                                              ds_3674
                                                              (con bytestring)
                                                              (lam
                                                                tn_3675
                                                                (con bytestring)
                                                                (lam
                                                                  amt_3676
                                                                  (con integer)
                                                                  {
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            {
                                                                              Nil_match_2745
                                                                              [
                                                                                [
                                                                                  [
                                                                                    Tuple3_3270
                                                                                    (con
                                                                                      bytestring
                                                                                    )
                                                                                  ]
                                                                                  (con
                                                                                    bytestring
                                                                                  )
                                                                                ]
                                                                                (con
                                                                                  integer
                                                                                )
                                                                              ]
                                                                            }
                                                                            ds_3671
                                                                          ]
                                                                          (all
                                                                            dead_3678
                                                                            (type)
                                                                            Bool_2694
                                                                          )
                                                                        }
                                                                        (abs
                                                                          dead_3679
                                                                          (type)
                                                                          {
                                                                            [
                                                                              [
                                                                                {
                                                                                  [
                                                                                    Bool_match_2697
                                                                                    [
                                                                                      [
                                                                                        equalsByteString_3339
                                                                                        tn_3675
                                                                                      ]
                                                                                      tn_3432
                                                                                    ]
                                                                                  ]
                                                                                  (all
                                                                                    dead_3681
                                                                                    (type)
                                                                                    Bool_2694
                                                                                  )
                                                                                }
                                                                                (abs
                                                                                  dead_3682
                                                                                  (type)
                                                                                  [
                                                                                    [
                                                                                      [
                                                                                        {
                                                                                          ifThenElse_2853
                                                                                          Bool_2694
                                                                                        }
                                                                                        [
                                                                                          [
                                                                                            equalsInteger_2860
                                                                                            amt_3676
                                                                                          ]
                                                                                          (con
                                                                                            integer
                                                                                            1
                                                                                          )
                                                                                        ]
                                                                                      ]
                                                                                      True_2695
                                                                                    ]
                                                                                    False_2696
                                                                                  ]
                                                                                )
                                                                              ]
                                                                              (abs
                                                                                dead_3683
                                                                                (type)
                                                                                False_2696
                                                                              )
                                                                            ]
                                                                            (all
                                                                              dead_3684
                                                                              (type)
                                                                              dead_3684
                                                                            )
                                                                          }
                                                                        )
                                                                      ]
                                                                      (lam
                                                                        ipv_3685
                                                                        [
                                                                          [
                                                                            [
                                                                              Tuple3_3270
                                                                              (con
                                                                                bytestring
                                                                              )
                                                                            ]
                                                                            (con
                                                                              bytestring
                                                                            )
                                                                          ]
                                                                          (con
                                                                            integer
                                                                          )
                                                                        ]
                                                                        (lam
                                                                          ipv_3686
                                                                          [
                                                                            List_2742
                                                                            [
                                                                              [
                                                                                [
                                                                                  Tuple3_3270
                                                                                  (con
                                                                                    bytestring
                                                                                  )
                                                                                ]
                                                                                (con
                                                                                  bytestring
                                                                                )
                                                                              ]
                                                                              (con
                                                                                integer
                                                                              )
                                                                            ]
                                                                          ]
                                                                          (abs
                                                                            dead_3687
                                                                            (type)
                                                                            False_2696
                                                                          )
                                                                        )
                                                                      )
                                                                    ]
                                                                    (all
                                                                      dead_3688
                                                                      (type)
                                                                      dead_3688
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
                                                  (all
                                                    dead_3689 (type) dead_3689
                                                  )
                                                }
                                              ]
                                              (all dead_3690 (type) Bool_2694)
                                            }
                                            (abs dead_3691 (type) True_2695)
                                          ]
                                          (abs
                                            dead_3692
                                            (type)
                                            [
                                              [
                                                { trace_2722 Bool_2694 }
                                                (con
                                                  string "wrong amount minted"
                                                )
                                              ]
                                              False_2696
                                            ]
                                          )
                                        ]
                                        (all dead_3693 (type) dead_3693)
                                      }
                                    )
                                  ]
                                  (abs dead_3694 (type) False_2696)
                                ]
                                (all dead_3695 (type) dead_3695)
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
                  (vardecl x_3241 ScriptContext_2843)
                  [
                    { error_2726 ScriptContext_2843 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3224 ScriptPurpose_2837)
                  [
                    { error_2726 ScriptPurpose_2837 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3131 DCert_2817)
                  [
                    { error_2726 DCert_2817 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2960 StakingCredential_2776)
                  [
                    { error_2726 StakingCredential_2776 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2941 Credential_2772)
                  [
                    { error_2726 Credential_2772 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl unsafeDataAsB_2864 (fun (con data) (con bytestring)))
                  (builtin unBData)
                )
                (termbind
                  (strict)
                  (vardecl
                    head_2855
                    (all a_2856 (type) (fun [ (con list) a_2856 ] a_2856))
                  )
                  (builtin headList)
                )
                (termbind
                  (strict)
                  (vardecl
                    snd_2849
                    (all
                      a_2850
                      (type)
                      (all
                        b_2851
                        (type)
                        (fun [ [ (con pair) a_2850 ] b_2851 ] b_2851)
                      )
                    )
                  )
                  (builtin sndPair)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataCredential_cunsafeFromBuiltinData_2945
                    (fun (con data) Credential_2772)
                  )
                  (lam
                    d_2931
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2932
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_2931 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2937 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2932
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2938 Credential_2772)
                        [ PubKeyCredential_2773 x_2937 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_2933 (con integer))
                        [
                          {
                            { fst_2857 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2932
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2943 Credential_2772)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2853
                                  (fun (con unit) Credential_2772)
                                }
                                [
                                  [ equalsInteger_2860 index_2933 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2939 (con unit) x_2938)
                            ]
                            (lam ds_2942 (con unit) x_2941)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2934 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2932
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2935 Credential_2772)
                        [ ScriptCredential_2774 x_2934 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2853 (fun (con unit) Credential_2772)
                              }
                              [
                                [ equalsInteger_2860 index_2933 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2936 (con unit) x_2935)
                          ]
                          (lam ds_2944 (con unit) x_2943)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl unsafeDataAsI_2863 (fun (con data) (con integer)))
                  (builtin unIData)
                )
                (termbind
                  (strict)
                  (vardecl
                    tail_2847
                    (all
                      a_2848
                      (type)
                      (fun [ (con list) a_2848 ] [ (con list) a_2848 ])
                    )
                  )
                  (builtin tailList)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                    (fun (con data) StakingCredential_2776)
                  )
                  (lam
                    d_2946
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2947
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_2946 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2956 Credential_2772)
                        [
                          fUnsafeFromDataCredential_cunsafeFromBuiltinData_2945
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2947
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2957 StakingCredential_2776)
                        [ StakingHash_2777 x_2956 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_2948 (con integer))
                        [
                          {
                            { fst_2857 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2947
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2962 StakingCredential_2776)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2853
                                  (fun (con unit) StakingCredential_2776)
                                }
                                [
                                  [ equalsInteger_2860 index_2948 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2958 (con unit) x_2957)
                            ]
                            (lam ds_2961 (con unit) x_2960)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2949 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2947
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2950 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_2949 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2953 (con integer))
                        [
                          unsafeDataAsI_2863
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_2950 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2952 (con integer))
                        [
                          unsafeDataAsI_2863 [ { head_2855 (con data) } t_2950 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2951 (con integer))
                        [
                          unsafeDataAsI_2863 [ { head_2855 (con data) } t_2949 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2954 StakingCredential_2776)
                        [ [ [ StakingPtr_2778 x_2951 ] x_2952 ] x_2953 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2853
                                (fun (con unit) StakingCredential_2776)
                              }
                              [
                                [ equalsInteger_2860 index_2948 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2955 (con unit) x_2954)
                          ]
                          (lam ds_2963 (con unit) x_2962)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataDCert_cunsafeFromBuiltinData_3145
                    (fun (con data) DCert_2817)
                  )
                  (lam
                    d_3104
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3105
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3104 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3127 StakingCredential_2776)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3105
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3128 DCert_2817)
                        [ DCertDelegRegKey_2820 x_3127 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_3106 (con integer))
                        [
                          {
                            { fst_2857 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3105
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3133 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_3129 (con unit) x_3128)
                            ]
                            (lam ds_3132 (con unit) x_3131)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3124 StakingCredential_2776)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3105
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3125 DCert_2817)
                        [ DCertDelegDeRegKey_2818 x_3124 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3135 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 1)
                                ]
                              ]
                              (lam ds_3126 (con unit) x_3125)
                            ]
                            (lam ds_3134 (con unit) x_3133)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3119 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3105
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3121 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3119 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3120 StakingCredential_2776)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                          [ { head_2855 (con data) } t_3119 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3122 DCert_2817)
                        [ [ DCertDelegDelegate_2819 x_3120 ] x_3121 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3137 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 2)
                                ]
                              ]
                              (lam ds_3123 (con unit) x_3122)
                            ]
                            (lam ds_3136 (con unit) x_3135)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3114 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3105
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3116 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3114 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3115 (con bytestring))
                        [
                          unsafeDataAsB_2864 [ { head_2855 (con data) } t_3114 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3117 DCert_2817)
                        [ [ DCertPoolRegister_2823 x_3115 ] x_3116 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3139 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 3)
                                ]
                              ]
                              (lam ds_3118 (con unit) x_3117)
                            ]
                            (lam ds_3138 (con unit) x_3137)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3109 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3105
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3111 (con integer))
                        [
                          unsafeDataAsI_2863
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3109 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3110 (con bytestring))
                        [
                          unsafeDataAsB_2864 [ { head_2855 (con data) } t_3109 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3112 DCert_2817)
                        [ [ DCertPoolRetire_2824 x_3110 ] x_3111 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3141 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 4)
                                ]
                              ]
                              (lam ds_3113 (con unit) x_3112)
                            ]
                            (lam ds_3140 (con unit) x_3139)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3143 DCert_2817)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) DCert_2817) }
                                [
                                  [ equalsInteger_2860 index_3106 ]
                                  (con integer 5)
                                ]
                              ]
                              (lam ds_3108 (con unit) DCertGenesis_2821)
                            ]
                            (lam ds_3142 (con unit) x_3141)
                          ]
                          unitval_2721
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) DCert_2817) }
                              [
                                [ equalsInteger_2860 index_3106 ]
                                (con integer 6)
                              ]
                            ]
                            (lam ds_3107 (con unit) DCertMir_2822)
                          ]
                          (lam ds_3144 (con unit) x_3143)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3011 TxOutRef_2791)
                  [
                    { error_2726 TxOutRef_2791 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3000 (con bytestring))
                  [
                    { error_2726 (con bytestring) }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxId_cunsafeFromBuiltinData_3002
                    (fun (con data) (con bytestring))
                  )
                  (lam
                    d_2995
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2996
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_2995 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2997 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_2996
                            ]
                          ]
                        ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2853
                                (fun (con unit) (con bytestring))
                              }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2996
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2998 (con unit) x_2997)
                          ]
                          (lam ds_3001 (con unit) x_3000)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_3013
                    (fun (con data) TxOutRef_2791)
                  )
                  (lam
                    d_3003
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3004
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3003 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3005 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3004
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3007 (con integer))
                        [
                          unsafeDataAsI_2863
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3005 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3006 (con bytestring))
                        [
                          fUnsafeFromDataTxId_cunsafeFromBuiltinData_3002
                          [ { head_2855 (con data) } t_3005 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3008 TxOutRef_2791)
                        [ [ TxOutRef_2792 x_3006 ] x_3007 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) TxOutRef_2791) }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3004
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3009 (con unit) x_3008)
                          ]
                          (lam ds_3012 (con unit) x_3011)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3232
                    (fun (con data) ScriptPurpose_2837)
                  )
                  (lam
                    d_3208
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3209
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3208 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3220 (con bytestring))
                        [
                          unsafeDataAsB_2864
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3209
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3221 ScriptPurpose_2837)
                        [ Minting_2839 x_3220 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl index_3210 (con integer))
                        [
                          {
                            { fst_2857 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3209
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3226 ScriptPurpose_2837)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2853
                                  (fun (con unit) ScriptPurpose_2837)
                                }
                                [
                                  [ equalsInteger_2860 index_3210 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_3222 (con unit) x_3221)
                            ]
                            (lam ds_3225 (con unit) x_3224)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3217 TxOutRef_2791)
                        [
                          fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_3013
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3209
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3218 ScriptPurpose_2837)
                        [ Spending_2841 x_3217 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3228 ScriptPurpose_2837)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2853
                                  (fun (con unit) ScriptPurpose_2837)
                                }
                                [
                                  [ equalsInteger_2860 index_3210 ]
                                  (con integer 1)
                                ]
                              ]
                              (lam ds_3219 (con unit) x_3218)
                            ]
                            (lam ds_3227 (con unit) x_3226)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3214 StakingCredential_2776)
                        [
                          fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3209
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3215 ScriptPurpose_2837)
                        [ Rewarding_2840 x_3214 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3230 ScriptPurpose_2837)
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_2853
                                  (fun (con unit) ScriptPurpose_2837)
                                }
                                [
                                  [ equalsInteger_2860 index_3210 ]
                                  (con integer 2)
                                ]
                              ]
                              (lam ds_3216 (con unit) x_3215)
                            ]
                            (lam ds_3229 (con unit) x_3228)
                          ]
                          unitval_2721
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3211 DCert_2817)
                        [
                          fUnsafeFromDataDCert_cunsafeFromBuiltinData_3145
                          [
                            { head_2855 (con data) }
                            [
                              {
                                { snd_2849 (con integer) }
                                [ (con list) (con data) ]
                              }
                              tup_3209
                            ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3212 ScriptPurpose_2837)
                        [ Certifying_2838 x_3211 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2853
                                (fun (con unit) ScriptPurpose_2837)
                              }
                              [
                                [ equalsInteger_2860 index_3210 ]
                                (con integer 3)
                              ]
                            ]
                            (lam ds_3213 (con unit) x_3212)
                          ]
                          (lam ds_3231 (con unit) x_3230)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3205 TxInfo_2826)
                  [
                    { error_2726 TxInfo_2826 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_3164
                    (all
                      a_3165
                      (type)
                      (all
                        b_3166
                        (type)
                        (fun
                          [ (lam a_3167 (type) (fun (con data) a_3167)) a_3165 ]
                          (fun
                            [
                              (lam a_3168 (type) (fun (con data) a_3168)) b_3166
                            ]
                            (fun (con data) [ [ Tuple2_2749 a_3165 ] b_3166 ])
                          )
                        )
                      )
                    )
                  )
                  (abs
                    a_3148
                    (type)
                    (abs
                      b_3149
                      (type)
                      (let
                        (nonrec)
                        (termbind
                          (nonstrict)
                          (vardecl x_3162 [ [ Tuple2_2749 a_3148 ] b_3149 ])
                          [
                            { error_2726 [ [ Tuple2_2749 a_3148 ] b_3149 ] }
                            [
                              {
                                [
                                  Unit_match_2732
                                  [
                                    [
                                      { trace_2722 Unit_2730 }
                                      reconstructCaseError_2852
                                    ]
                                    Unit_2731
                                  ]
                                ]
                                (con unit)
                              }
                              unitval_2721
                            ]
                          ]
                        )
                        (lam
                          dUnsafeFromData_3150
                          [ (lam a_3151 (type) (fun (con data) a_3151)) a_3148 ]
                          (lam
                            dUnsafeFromData_3152
                            [
                              (lam a_3153 (type) (fun (con data) a_3153)) b_3149
                            ]
                            (lam
                              d_3154
                              (con data)
                              (let
                                (nonrec)
                                (termbind
                                  (nonstrict)
                                  (vardecl
                                    tup_3155
                                    [
                                      [ (con pair) (con integer) ]
                                      [ (con list) (con data) ]
                                    ]
                                  )
                                  [ unsafeDataAsConstr_2846 d_3154 ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl t_3156 [ (con list) (con data) ])
                                  [
                                    {
                                      { snd_2849 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3155
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl x_3158 b_3149)
                                  [
                                    dUnsafeFromData_3152
                                    [
                                      { head_2855 (con data) }
                                      [ { tail_2847 (con data) } t_3156 ]
                                    ]
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl x_3157 a_3148)
                                  [
                                    dUnsafeFromData_3150
                                    [ { head_2855 (con data) } t_3156 ]
                                  ]
                                )
                                (termbind
                                  (nonstrict)
                                  (vardecl
                                    x_3159 [ [ Tuple2_2749 a_3148 ] b_3149 ]
                                  )
                                  [
                                    [ { { Tuple2_2750 a_3148 } b_3149 } x_3157 ]
                                    x_3158
                                  ]
                                )
                                [
                                  [
                                    [
                                      [
                                        {
                                          ifThenElse_2853
                                          (fun
                                            (con unit)
                                            [ [ Tuple2_2749 a_3148 ] b_3149 ]
                                          )
                                        }
                                        [
                                          [
                                            equalsInteger_2860
                                            [
                                              {
                                                { fst_2857 (con integer) }
                                                [ (con list) (con data) ]
                                              }
                                              tup_3155
                                            ]
                                          ]
                                          (con integer 0)
                                        ]
                                      ]
                                      (lam ds_3160 (con unit) x_3159)
                                    ]
                                    (lam ds_3163 (con unit) x_3162)
                                  ]
                                  unitval_2721
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
                    fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_3147
                    (fun (con data) (con data))
                  )
                  (lam d_3146 (con data) d_3146)
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3051 Bool_2694)
                  [
                    { error_2726 Bool_2694 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataBool_cunsafeFromBuiltinData_3055
                    (fun (con data) Bool_2694)
                  )
                  (lam
                    d_3046
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl index_3047 (con integer))
                        [
                          {
                            { fst_2857 (con integer) } [ (con list) (con data) ]
                          }
                          [ unsafeDataAsConstr_2846 d_3046 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3053 Bool_2694)
                        [
                          [
                            [
                              [
                                { ifThenElse_2853 (fun (con unit) Bool_2694) }
                                [
                                  [ equalsInteger_2860 index_3047 ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_3049 (con unit) False_2696)
                            ]
                            (lam ds_3052 (con unit) x_3051)
                          ]
                          unitval_2721
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) Bool_2694) }
                              [
                                [ equalsInteger_2860 index_3047 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_3048 (con unit) True_2695)
                          ]
                          (lam ds_3054 (con unit) x_3053)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_3043
                    (all
                      a_3044
                      (type)
                      (fun
                        [ (lam a_3045 (type) (fun (con data) a_3045)) a_3044 ]
                        (fun (con data) [ Extended_2798 a_3044 ])
                      )
                    )
                  )
                  (abs
                    a_3025
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_3037 [ Extended_2798 a_3025 ])
                        [
                          { error_2726 [ Extended_2798 a_3025 ] }
                          [
                            {
                              [
                                Unit_match_2732
                                [
                                  [
                                    { trace_2722 Unit_2730 }
                                    reconstructCaseError_2852
                                  ]
                                  Unit_2731
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2721
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_3026
                        [ (lam a_3027 (type) (fun (con data) a_3027)) a_3025 ]
                        (lam
                          d_3028
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_3029
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2846 d_3028 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl index_3030 (con integer))
                              [
                                {
                                  { fst_2857 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_3029
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3039 [ Extended_2798 a_3025 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2853
                                        (fun
                                          (con unit) [ Extended_2798 a_3025 ]
                                        )
                                      }
                                      [
                                        [ equalsInteger_2860 index_3030 ]
                                        (con integer 0)
                                      ]
                                    ]
                                    (lam
                                      ds_3035 (con unit) { NegInf_2800 a_3025 }
                                    )
                                  ]
                                  (lam ds_3038 (con unit) x_3037)
                                ]
                                unitval_2721
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3032 a_3025)
                              [
                                dUnsafeFromData_3026
                                [
                                  { head_2855 (con data) }
                                  [
                                    {
                                      { snd_2849 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3029
                                  ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3033 [ Extended_2798 a_3025 ])
                              [ { Finite_2799 a_3025 } x_3032 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3041 [ Extended_2798 a_3025 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2853
                                        (fun
                                          (con unit) [ Extended_2798 a_3025 ]
                                        )
                                      }
                                      [
                                        [ equalsInteger_2860 index_3030 ]
                                        (con integer 1)
                                      ]
                                    ]
                                    (lam ds_3034 (con unit) x_3033)
                                  ]
                                  (lam ds_3040 (con unit) x_3039)
                                ]
                                unitval_2721
                              ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2853
                                      (fun (con unit) [ Extended_2798 a_3025 ])
                                    }
                                    [
                                      [ equalsInteger_2860 index_3030 ]
                                      (con integer 2)
                                    ]
                                  ]
                                  (lam
                                    ds_3031 (con unit) { PosInf_2801 a_3025 }
                                  )
                                ]
                                (lam ds_3042 (con unit) x_3041)
                              ]
                              unitval_2721
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
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_3085
                    (all
                      a_3086
                      (type)
                      (fun
                        [ (lam a_3087 (type) (fun (con data) a_3087)) a_3086 ]
                        (fun (con data) [ UpperBound_2804 a_3086 ])
                      )
                    )
                  )
                  (abs
                    a_3072
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_3083 [ UpperBound_2804 a_3072 ])
                        [
                          { error_2726 [ UpperBound_2804 a_3072 ] }
                          [
                            {
                              [
                                Unit_match_2732
                                [
                                  [
                                    { trace_2722 Unit_2730 }
                                    reconstructCaseError_2852
                                  ]
                                  Unit_2731
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2721
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_3073
                        [ (lam a_3074 (type) (fun (con data) a_3074)) a_3072 ]
                        (lam
                          d_3075
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_3076
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2846 d_3075 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_3077 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2849 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_3076
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3079 Bool_2694)
                              [
                                fUnsafeFromDataBool_cunsafeFromBuiltinData_3055
                                [
                                  { head_2855 (con data) }
                                  [ { tail_2847 (con data) } t_3077 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3078 [ Extended_2798 a_3072 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_3043
                                    a_3072
                                  }
                                  dUnsafeFromData_3073
                                ]
                                [ { head_2855 (con data) } t_3077 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3080 [ UpperBound_2804 a_3072 ])
                              [ [ { UpperBound_2805 a_3072 } x_3078 ] x_3079 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2853
                                      (fun
                                        (con unit) [ UpperBound_2804 a_3072 ]
                                      )
                                    }
                                    [
                                      [
                                        equalsInteger_2860
                                        [
                                          {
                                            { fst_2857 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_3076
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_3081 (con unit) x_3080)
                                ]
                                (lam ds_3084 (con unit) x_3083)
                              ]
                              unitval_2721
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
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_3069
                    (all
                      a_3070
                      (type)
                      (fun
                        [ (lam a_3071 (type) (fun (con data) a_3071)) a_3070 ]
                        (fun (con data) [ LowerBound_2808 a_3070 ])
                      )
                    )
                  )
                  (abs
                    a_3056
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_3067 [ LowerBound_2808 a_3056 ])
                        [
                          { error_2726 [ LowerBound_2808 a_3056 ] }
                          [
                            {
                              [
                                Unit_match_2732
                                [
                                  [
                                    { trace_2722 Unit_2730 }
                                    reconstructCaseError_2852
                                  ]
                                  Unit_2731
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2721
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_3057
                        [ (lam a_3058 (type) (fun (con data) a_3058)) a_3056 ]
                        (lam
                          d_3059
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_3060
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2846 d_3059 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_3061 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2849 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_3060
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3063 Bool_2694)
                              [
                                fUnsafeFromDataBool_cunsafeFromBuiltinData_3055
                                [
                                  { head_2855 (con data) }
                                  [ { tail_2847 (con data) } t_3061 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3062 [ Extended_2798 a_3056 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataExtended_cunsafeFromBuiltinData_3043
                                    a_3056
                                  }
                                  dUnsafeFromData_3057
                                ]
                                [ { head_2855 (con data) } t_3061 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3064 [ LowerBound_2808 a_3056 ])
                              [ [ { LowerBound_2809 a_3056 } x_3062 ] x_3063 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2853
                                      (fun
                                        (con unit) [ LowerBound_2808 a_3056 ]
                                      )
                                    }
                                    [
                                      [
                                        equalsInteger_2860
                                        [
                                          {
                                            { fst_2857 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_3060
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_3065 (con unit) x_3064)
                                ]
                                (lam ds_3068 (con unit) x_3067)
                              ]
                              unitval_2721
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
                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_3101
                    (all
                      a_3102
                      (type)
                      (fun
                        [ (lam a_3103 (type) (fun (con data) a_3103)) a_3102 ]
                        (fun (con data) [ Interval_2812 a_3102 ])
                      )
                    )
                  )
                  (abs
                    a_3088
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_3099 [ Interval_2812 a_3088 ])
                        [
                          { error_2726 [ Interval_2812 a_3088 ] }
                          [
                            {
                              [
                                Unit_match_2732
                                [
                                  [
                                    { trace_2722 Unit_2730 }
                                    reconstructCaseError_2852
                                  ]
                                  Unit_2731
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2721
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_3089
                        [ (lam a_3090 (type) (fun (con data) a_3090)) a_3088 ]
                        (lam
                          d_3091
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_3092
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2846 d_3091 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl t_3093 [ (con list) (con data) ])
                              [
                                {
                                  { snd_2849 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_3092
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3095 [ UpperBound_2804 a_3088 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_3085
                                    a_3088
                                  }
                                  dUnsafeFromData_3089
                                ]
                                [
                                  { head_2855 (con data) }
                                  [ { tail_2847 (con data) } t_3093 ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3094 [ LowerBound_2808 a_3088 ])
                              [
                                [
                                  {
                                    fUnsafeFromDataInterval_cunsafeFromBuiltinData_3069
                                    a_3088
                                  }
                                  dUnsafeFromData_3089
                                ]
                                [ { head_2855 (con data) } t_3093 ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_3096 [ Interval_2812 a_3088 ])
                              [ [ { Interval_2813 a_3088 } x_3094 ] x_3095 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2853
                                      (fun (con unit) [ Interval_2812 a_3088 ])
                                    }
                                    [
                                      [
                                        equalsInteger_2860
                                        [
                                          {
                                            { fst_2857 (con integer) }
                                            [ (con list) (con data) ]
                                          }
                                          tup_3092
                                        ]
                                      ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_3097 (con unit) x_3096)
                                ]
                                (lam ds_3100 (con unit) x_3099)
                              ]
                              unitval_2721
                            ]
                          )
                        )
                      )
                    )
                  )
                )
                (termbind
                  (nonstrict)
                  (vardecl x_3022 TxInInfo_2794)
                  [
                    { error_2726 TxInInfo_2794 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2992 TxOut_2783)
                  [
                    { error_2726 TxOut_2783 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (nonstrict)
                  (vardecl x_2973 Address_2780)
                  [
                    { error_2726 Address_2780 }
                    [
                      {
                        [
                          Unit_match_2732
                          [
                            [
                              { trace_2722 Unit_2730 } reconstructCaseError_2852
                            ]
                            Unit_2731
                          ]
                        ]
                        (con unit)
                      }
                      unitval_2721
                    ]
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2928
                    (all
                      a_2929
                      (type)
                      (fun
                        [ (lam a_2930 (type) (fun (con data) a_2930)) a_2929 ]
                        (fun (con data) [ Maybe_2764 a_2929 ])
                      )
                    )
                  )
                  (abs
                    a_2913
                    (type)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl x_2924 [ Maybe_2764 a_2913 ])
                        [
                          { error_2726 [ Maybe_2764 a_2913 ] }
                          [
                            {
                              [
                                Unit_match_2732
                                [
                                  [
                                    { trace_2722 Unit_2730 }
                                    reconstructCaseError_2852
                                  ]
                                  Unit_2731
                                ]
                              ]
                              (con unit)
                            }
                            unitval_2721
                          ]
                        ]
                      )
                      (lam
                        dUnsafeFromData_2914
                        [ (lam a_2915 (type) (fun (con data) a_2915)) a_2913 ]
                        (lam
                          d_2916
                          (con data)
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl
                                tup_2917
                                [
                                  [ (con pair) (con integer) ]
                                  [ (con list) (con data) ]
                                ]
                              )
                              [ unsafeDataAsConstr_2846 d_2916 ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl index_2918 (con integer))
                              [
                                {
                                  { fst_2857 (con integer) }
                                  [ (con list) (con data) ]
                                }
                                tup_2917
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2926 [ Maybe_2764 a_2913 ])
                              [
                                [
                                  [
                                    [
                                      {
                                        ifThenElse_2853
                                        (fun (con unit) [ Maybe_2764 a_2913 ])
                                      }
                                      [
                                        [ equalsInteger_2860 index_2918 ]
                                        (con integer 1)
                                      ]
                                    ]
                                    (lam
                                      ds_2922 (con unit) { Nothing_2766 a_2913 }
                                    )
                                  ]
                                  (lam ds_2925 (con unit) x_2924)
                                ]
                                unitval_2721
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2919 a_2913)
                              [
                                dUnsafeFromData_2914
                                [
                                  { head_2855 (con data) }
                                  [
                                    {
                                      { snd_2849 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2917
                                  ]
                                ]
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl x_2920 [ Maybe_2764 a_2913 ])
                              [ { Just_2765 a_2913 } x_2919 ]
                            )
                            [
                              [
                                [
                                  [
                                    {
                                      ifThenElse_2853
                                      (fun (con unit) [ Maybe_2764 a_2913 ])
                                    }
                                    [
                                      [ equalsInteger_2860 index_2918 ]
                                      (con integer 0)
                                    ]
                                  ]
                                  (lam ds_2921 (con unit) x_2920)
                                ]
                                (lam ds_2927 (con unit) x_2926)
                              ]
                              unitval_2721
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
                    fUnsafeFromDataAddress_cunsafeFromBuiltinData_2975
                    (fun (con data) Address_2780)
                  )
                  (lam
                    d_2965
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2966
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_2965 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2967 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2966
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2969 [ Maybe_2764 StakingCredential_2776 ])
                        [
                          [
                            {
                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2928
                              StakingCredential_2776
                            }
                            fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                          ]
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_2967 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2968 Credential_2772)
                        [
                          fUnsafeFromDataCredential_cunsafeFromBuiltinData_2945
                          [ { head_2855 (con data) } t_2967 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2970 Address_2780)
                        [ [ Address_2781 x_2968 ] x_2969 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) Address_2780) }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2966
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2971 (con unit) x_2970)
                          ]
                          (lam ds_2974 (con unit) x_2973)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMap_2884
                    (all
                      v_2885
                      (type)
                      (all
                        k_2886
                        (type)
                        (fun
                          Unit_2730
                          [ List_2742 [ [ Tuple2_2749 k_2886 ] v_2885 ] ]
                        )
                      )
                    )
                  )
                  (abs
                    v_2881
                    (type)
                    (abs
                      k_2882
                      (type)
                      (lam
                        ds_2883
                        Unit_2730
                        { Nil_2743 [ [ Tuple2_2749 k_2882 ] v_2881 ] }
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    unsafeDataAsMap_2880
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
                    chooseList_2866
                    (all
                      a_2867
                      (type)
                      (all
                        b_2868
                        (type)
                        (fun
                          [ (con list) a_2867 ] (fun b_2868 (fun b_2868 b_2868))
                        )
                      )
                    )
                  )
                  (builtin chooseList)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataMap_cunsafeFromBuiltinData_2903
                    (all
                      k_2904
                      (type)
                      (all
                        v_2905
                        (type)
                        (fun
                          [ (lam a_2906 (type) (fun (con data) a_2906)) k_2904 ]
                          (fun
                            [
                              (lam a_2907 (type) (fun (con data) a_2907)) v_2905
                            ]
                            (fun
                              (con data)
                              [
                                [
                                  (lam
                                    k_2908
                                    (type)
                                    (lam
                                      v_2909
                                      (type)
                                      [
                                        List_2742
                                        [ [ Tuple2_2749 k_2908 ] v_2909 ]
                                      ]
                                    )
                                  )
                                  k_2904
                                ]
                                v_2905
                              ]
                            )
                          )
                        )
                      )
                    )
                  )
                  (abs
                    k_2887
                    (type)
                    (abs
                      v_2888
                      (type)
                      (lam
                        dUnsafeFromData_2889
                        [ (lam a_2890 (type) (fun (con data) a_2890)) k_2887 ]
                        (lam
                          dUnsafeFromData_2891
                          [ (lam a_2892 (type) (fun (con data) a_2892)) v_2888 ]
                          (let
                            (rec)
                            (termbind
                              (strict)
                              (vardecl
                                go_2894
                                (fun
                                  [
                                    (con list)
                                    [ [ (con pair) (con data) ] (con data) ]
                                  ]
                                  [
                                    List_2742 [ [ Tuple2_2749 k_2887 ] v_2888 ]
                                  ]
                                )
                              )
                              (lam
                                l_2895
                                [
                                  (con list)
                                  [ [ (con pair) (con data) ] (con data) ]
                                ]
                                (let
                                  (nonrec)
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2900
                                      [
                                        List_2742
                                        [ [ Tuple2_2749 k_2887 ] v_2888 ]
                                      ]
                                    )
                                    [
                                      go_2894
                                      [
                                        {
                                          tail_2847
                                          [
                                            [ (con pair) (con data) ] (con data)
                                          ]
                                        }
                                        l_2895
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      tup_2896
                                      [ [ (con pair) (con data) ] (con data) ]
                                    )
                                    [
                                      {
                                        head_2855
                                        [ [ (con pair) (con data) ] (con data) ]
                                      }
                                      l_2895
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl lvl_2898 v_2888)
                                    [
                                      dUnsafeFromData_2891
                                      [
                                        { { snd_2849 (con data) } (con data) }
                                        tup_2896
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl lvl_2897 k_2887)
                                    [
                                      dUnsafeFromData_2889
                                      [
                                        { { fst_2857 (con data) } (con data) }
                                        tup_2896
                                      ]
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2899 [ [ Tuple2_2749 k_2887 ] v_2888 ]
                                    )
                                    [
                                      [
                                        { { Tuple2_2750 k_2887 } v_2888 }
                                        lvl_2897
                                      ]
                                      lvl_2898
                                    ]
                                  )
                                  (termbind
                                    (nonstrict)
                                    (vardecl
                                      lvl_2901
                                      [
                                        List_2742
                                        [ [ Tuple2_2749 k_2887 ] v_2888 ]
                                      ]
                                    )
                                    [
                                      [
                                        {
                                          Cons_2744
                                          [ [ Tuple2_2749 k_2887 ] v_2888 ]
                                        }
                                        lvl_2899
                                      ]
                                      lvl_2900
                                    ]
                                  )
                                  [
                                    [
                                      [
                                        [
                                          {
                                            {
                                              chooseList_2866
                                              [
                                                [ (con pair) (con data) ]
                                                (con data)
                                              ]
                                            }
                                            (fun
                                              Unit_2730
                                              [
                                                List_2742
                                                [
                                                  [ Tuple2_2749 k_2887 ] v_2888
                                                ]
                                              ]
                                            )
                                          }
                                          l_2895
                                        ]
                                        {
                                          { fUnsafeFromDataMap_2884 v_2888 }
                                          k_2887
                                        }
                                      ]
                                      (lam ds_2902 Unit_2730 lvl_2901)
                                    ]
                                    Unit_2731
                                  ]
                                )
                              )
                            )
                            (lam
                              d_2893
                              (con data)
                              [ go_2894 [ unsafeDataAsMap_2880 d_2893 ] ]
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
                    fUnsafeFromDataValue_2910
                    (fun
                      (con data)
                      [
                        [
                          (lam
                            k_2911
                            (type)
                            (lam
                              v_2912
                              (type)
                              [ List_2742 [ [ Tuple2_2749 k_2911 ] v_2912 ] ]
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
                          fUnsafeFromDataMap_cunsafeFromBuiltinData_2903
                          (con bytestring)
                        }
                        (con integer)
                      }
                      unsafeDataAsB_2864
                    ]
                    unsafeDataAsI_2863
                  ]
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2994
                    (fun (con data) TxOut_2783)
                  )
                  (lam
                    d_2976
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_2977
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_2976 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2978 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_2977
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_2979 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_2978 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2988 [ Maybe_2764 (con bytestring) ])
                        [
                          [
                            {
                              fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2928
                              (con bytestring)
                            }
                            unsafeDataAsB_2864
                          ]
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_2979 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_2983
                          [
                            [
                              (lam
                                k_2984
                                (type)
                                (lam
                                  v_2985
                                  (type)
                                  [
                                    List_2742 [ [ Tuple2_2749 k_2984 ] v_2985 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_2986
                                  (type)
                                  (lam
                                    v_2987
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_2986 ] v_2987 ]
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
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2903
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_2981
                                      (type)
                                      (lam
                                        v_2982
                                        (type)
                                        [
                                          List_2742
                                          [ [ Tuple2_2749 k_2981 ] v_2982 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2864
                            ]
                            fUnsafeFromDataValue_2910
                          ]
                          [ { head_2855 (con data) } t_2979 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2980 Address_2780)
                        [
                          fUnsafeFromDataAddress_cunsafeFromBuiltinData_2975
                          [ { head_2855 (con data) } t_2978 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_2989 TxOut_2783)
                        [ [ [ TxOut_2784 x_2980 ] x_2983 ] x_2988 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) TxOut_2783) }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_2977
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2990 (con unit) x_2989)
                          ]
                          (lam ds_2993 (con unit) x_2992)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3024
                    (fun (con data) TxInInfo_2794)
                  )
                  (lam
                    d_3014
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3015
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3014 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3016 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3015
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3018 TxOut_2783)
                        [
                          fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2994
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3016 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3017 TxOutRef_2791)
                        [
                          fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_3013
                          [ { head_2855 (con data) } t_3016 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3019 TxInInfo_2794)
                        [ [ TxInInfo_2795 x_3017 ] x_3018 ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) TxInInfo_2794) }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3015
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3020 (con unit) x_3019)
                          ]
                          (lam ds_3023 (con unit) x_3022)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    unsafeDataAsList_2865
                    (fun (con data) [ (con list) (con data) ])
                  )
                  (builtin unListData)
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                    (all
                      a_2878
                      (type)
                      (fun
                        [ (lam a_2879 (type) (fun (con data) a_2879)) a_2878 ]
                        (fun (con data) [ List_2742 a_2878 ])
                      )
                    )
                  )
                  (abs
                    a_2869
                    (type)
                    (lam
                      dUnsafeFromData_2870
                      [ (lam a_2871 (type) (fun (con data) a_2871)) a_2869 ]
                      (let
                        (rec)
                        (termbind
                          (strict)
                          (vardecl
                            go_2873
                            (fun [ (con list) (con data) ] [ List_2742 a_2869 ])
                          )
                          (lam
                            l_2874
                            [ (con list) (con data) ]
                            [
                              [
                                [
                                  [
                                    {
                                      { chooseList_2866 (con data) }
                                      (fun Unit_2730 [ List_2742 a_2869 ])
                                    }
                                    l_2874
                                  ]
                                  (lam ds_2875 Unit_2730 { Nil_2743 a_2869 })
                                ]
                                (lam
                                  ds_2876
                                  Unit_2730
                                  [
                                    [
                                      { Cons_2744 a_2869 }
                                      [
                                        dUnsafeFromData_2870
                                        [ { head_2855 (con data) } l_2874 ]
                                      ]
                                    ]
                                    [
                                      go_2873
                                      [ { tail_2847 (con data) } l_2874 ]
                                    ]
                                  ]
                                )
                              ]
                              Unit_2731
                            ]
                          )
                        )
                        (lam
                          d_2872
                          (con data)
                          [ go_2873 [ unsafeDataAsList_2865 d_2872 ] ]
                        )
                      )
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3207
                    (fun (con data) TxInfo_2826)
                  )
                  (lam
                    d_3169
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3170
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3169 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3171 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3170
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3172 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3171 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3173 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3172 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3174 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3173 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3175 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3174 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3176 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3175 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3177 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3176 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3178 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3177 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3179 [ (con list) (con data) ])
                        [ { tail_2847 (con data) } t_3178 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3201 (con bytestring))
                        [
                          fUnsafeFromDataTxId_cunsafeFromBuiltinData_3002
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3179 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3200
                          [
                            List_2742
                            [ [ Tuple2_2749 (con bytestring) ] (con data) ]
                          ]
                        )
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              [ [ Tuple2_2749 (con bytestring) ] (con data) ]
                            }
                            [
                              [
                                {
                                  {
                                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_3164
                                    (con bytestring)
                                  }
                                  (con data)
                                }
                                unsafeDataAsB_2864
                              ]
                              fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_3147
                            ]
                          ]
                          [ { head_2855 (con data) } t_3179 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3199 [ List_2742 (con bytestring) ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              (con bytestring)
                            }
                            unsafeDataAsB_2864
                          ]
                          [ { head_2855 (con data) } t_3178 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3198 [ Interval_2812 (con integer) ])
                        [
                          [
                            {
                              fUnsafeFromDataInterval_cunsafeFromBuiltinData_3101
                              (con integer)
                            }
                            unsafeDataAsI_2863
                          ]
                          [ { head_2855 (con data) } t_3177 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3197
                          [
                            List_2742
                            [
                              [ Tuple2_2749 StakingCredential_2776 ]
                              (con integer)
                            ]
                          ]
                        )
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              [
                                [ Tuple2_2749 StakingCredential_2776 ]
                                (con integer)
                              ]
                            }
                            [
                              [
                                {
                                  {
                                    fUnsafeFromDataTuple2_cunsafeFromBuiltinData_3164
                                    StakingCredential_2776
                                  }
                                  (con integer)
                                }
                                fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2964
                              ]
                              unsafeDataAsI_2863
                            ]
                          ]
                          [ { head_2855 (con data) } t_3176 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3196 [ List_2742 DCert_2817 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              DCert_2817
                            }
                            fUnsafeFromDataDCert_cunsafeFromBuiltinData_3145
                          ]
                          [ { head_2855 (con data) } t_3175 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3191
                          [
                            [
                              (lam
                                k_3192
                                (type)
                                (lam
                                  v_3193
                                  (type)
                                  [
                                    List_2742 [ [ Tuple2_2749 k_3192 ] v_3193 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_3194
                                  (type)
                                  (lam
                                    v_3195
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_3194 ] v_3195 ]
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
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2903
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_3189
                                      (type)
                                      (lam
                                        v_3190
                                        (type)
                                        [
                                          List_2742
                                          [ [ Tuple2_2749 k_3189 ] v_3190 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2864
                            ]
                            fUnsafeFromDataValue_2910
                          ]
                          [ { head_2855 (con data) } t_3174 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl
                          x_3184
                          [
                            [
                              (lam
                                k_3185
                                (type)
                                (lam
                                  v_3186
                                  (type)
                                  [
                                    List_2742 [ [ Tuple2_2749 k_3185 ] v_3186 ]
                                  ]
                                )
                              )
                              (con bytestring)
                            ]
                            [
                              [
                                (lam
                                  k_3187
                                  (type)
                                  (lam
                                    v_3188
                                    (type)
                                    [
                                      List_2742
                                      [ [ Tuple2_2749 k_3187 ] v_3188 ]
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
                                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2903
                                  (con bytestring)
                                }
                                [
                                  [
                                    (lam
                                      k_3182
                                      (type)
                                      (lam
                                        v_3183
                                        (type)
                                        [
                                          List_2742
                                          [ [ Tuple2_2749 k_3182 ] v_3183 ]
                                        ]
                                      )
                                    )
                                    (con bytestring)
                                  ]
                                  (con integer)
                                ]
                              }
                              unsafeDataAsB_2864
                            ]
                            fUnsafeFromDataValue_2910
                          ]
                          [ { head_2855 (con data) } t_3173 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3181 [ List_2742 TxOut_2783 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              TxOut_2783
                            }
                            fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2994
                          ]
                          [ { head_2855 (con data) } t_3172 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3180 [ List_2742 TxInInfo_2794 ])
                        [
                          [
                            {
                              fUnsafeFromDataNil_cunsafeFromBuiltinData_2877
                              TxInInfo_2794
                            }
                            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3024
                          ]
                          [ { head_2855 (con data) } t_3171 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3202 TxInfo_2826)
                        [
                          [
                            [
                              [
                                [
                                  [
                                    [
                                      [
                                        [ [ TxInfo_2827 x_3180 ] x_3181 ] x_3184
                                      ]
                                      x_3191
                                    ]
                                    x_3196
                                  ]
                                  x_3197
                                ]
                                x_3198
                              ]
                              x_3199
                            ]
                            x_3200
                          ]
                          x_3201
                        ]
                      )
                      [
                        [
                          [
                            [
                              { ifThenElse_2853 (fun (con unit) TxInfo_2826) }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3170
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3203 (con unit) x_3202)
                          ]
                          (lam ds_3206 (con unit) x_3205)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3243
                    (fun (con data) ScriptContext_2843)
                  )
                  (lam
                    d_3233
                    (con data)
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl
                          tup_3234
                          [
                            [ (con pair) (con integer) ]
                            [ (con list) (con data) ]
                          ]
                        )
                        [ unsafeDataAsConstr_2846 d_3233 ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl t_3235 [ (con list) (con data) ])
                        [
                          {
                            { snd_2849 (con integer) } [ (con list) (con data) ]
                          }
                          tup_3234
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3237 ScriptPurpose_2837)
                        [
                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3232
                          [
                            { head_2855 (con data) }
                            [ { tail_2847 (con data) } t_3235 ]
                          ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3236 TxInfo_2826)
                        [
                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3207
                          [ { head_2855 (con data) } t_3235 ]
                        ]
                      )
                      (termbind
                        (nonstrict)
                        (vardecl x_3238 ScriptContext_2843)
                        [ [ ScriptContext_2844 x_3236 ] x_3237 ]
                      )
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_2853
                                (fun (con unit) ScriptContext_2843)
                              }
                              [
                                [
                                  equalsInteger_2860
                                  [
                                    {
                                      { fst_2857 (con integer) }
                                      [ (con list) (con data) ]
                                    }
                                    tup_3234
                                  ]
                                ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_3239 (con unit) x_3238)
                          ]
                          (lam ds_3242 (con unit) x_3241)
                        ]
                        unitval_2721
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl checkHasFailedError_2738 (con string))
                  (con string "PT5")
                )
                (termbind
                  (strict)
                  (vardecl
                    traceError_2736
                    (all a_2737 (type) (fun (con string) a_2737))
                  )
                  (abs
                    a_2733
                    (type)
                    (lam
                      str_2734
                      (con string)
                      [
                        { error_2726 a_2733 }
                        [
                          {
                            [
                              Unit_match_2732
                              [
                                [ { trace_2722 Unit_2730 } str_2734 ] Unit_2731
                              ]
                            ]
                            (con unit)
                          }
                          unitval_2721
                        ]
                      ]
                    )
                  )
                )
                (termbind
                  (strict)
                  (vardecl
                    wrapMintingPolicy_3255
                    (all
                      r_3256
                      (type)
                      (fun
                        [ (lam a_3257 (type) (fun (con data) a_3257)) r_3256 ]
                        (fun
                          (fun r_3256 (fun ScriptContext_2843 Bool_2694))
                          (fun (con data) (fun (con data) Unit_2730))
                        )
                      )
                    )
                  )
                  (abs
                    r_3244
                    (type)
                    (lam
                      dUnsafeFromData_3245
                      [ (lam a_3246 (type) (fun (con data) a_3246)) r_3244 ]
                      (lam
                        f_3247
                        (fun r_3244 (fun ScriptContext_2843 Bool_2694))
                        (lam
                          r_3248
                          (con data)
                          (lam
                            p_3249
                            (con data)
                            {
                              [
                                [
                                  {
                                    [
                                      Bool_match_2697
                                      [
                                        [
                                          f_3247 [ dUnsafeFromData_3245 r_3248 ]
                                        ]
                                        [
                                          fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_3243
                                          p_3249
                                        ]
                                      ]
                                    ]
                                    (all dead_3251 (type) Unit_2730)
                                  }
                                  (abs dead_3252 (type) Unit_2731)
                                ]
                                (abs
                                  dead_3253
                                  (type)
                                  [
                                    { traceError_2736 Unit_2730 }
                                    checkHasFailedError_2738
                                  ]
                                )
                              ]
                              (all dead_3254 (type) dead_3254)
                            }
                          )
                        )
                      )
                    )
                  )
                )
                (lam
                  oref_3746
                  TxOutRef_2791
                  (lam
                    tn_3747
                    (con bytestring)
                    [
                      [
                        {
                          {
                            bad_name_3743
                            (fun Unit_2730 (fun ScriptContext_2843 Bool_2694))
                          }
                          (fun (con data) (fun (con data) Unit_2730))
                        }
                        [
                          { wrapMintingPolicy_3255 Unit_2730 }
                          fUnsafeFromDataUnit_3737
                        ]
                      ]
                      [ [ mkPolicy_3696 oref_3746 ] tn_3747 ]
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