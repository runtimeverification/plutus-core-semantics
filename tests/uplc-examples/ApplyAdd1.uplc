(program
  1.0.0
  [
    [
      (force (force (delay (delay (lam f (lam x [ f x ]))))))
      [
        (builtin addInteger)
        [
          (lam x0 (con integer 2))
          [
            (lam
              x1
              [
                [
                  (builtin multiplyInteger)
                  [
                    [ (builtin multiplyInteger) (con integer 0) ]
                    (con integer 0)
                  ]
                ]
                [
                  [ (builtin subtractInteger) (con integer 2) ] (con integer 0)
                ]
              ]
            )
            [
              (lam
                x2
                [
                  [ (builtin lessThanInteger) (con integer 0) ] (con integer 3)
                ]
              )
              [ [ (builtin equalsInteger) (con integer 3) ] (con integer 1) ]
            ]
          ]
        ]
      ]
    ]
    (con integer 1)
  ]
)
