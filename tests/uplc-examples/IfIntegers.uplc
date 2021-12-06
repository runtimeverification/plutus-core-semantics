(program
  1.0.0
  [
    [
      [
        (force
          (delay
            (lam
              b
              (lam
                x
                (lam
                  y
                  [
                    [ [ [ (force (builtin ifThenElse)) b ] x ] y ] (con unit ())
                  ]
                )
              )
            )
          )
        )
        (con bool False)
      ]
      [
        (force (force (delay (delay (lam x (lam y x))))))
        [
          (lam
            x0
            [
              [
                (builtin addInteger)
                [
                  [
                    (builtin multiplyInteger)
                    [
                      [ (builtin multiplyInteger) (con integer 2) ]
                      (con integer 0)
                    ]
                  ]
                  [
                    [ (builtin multiplyInteger) (con integer 1) ]
                    (con integer 3)
                  ]
                ]
              ]
              (con integer 1)
            ]
          )
          [
            [
              (builtin appendByteString)
              [
                (lam x1 [ (builtin sha3_256) (con bytestring #6f6f72) ])
                [
                  [ (builtin lessThanEqualsInteger) (con integer 3) ]
                  (con integer 1)
                ]
              ]
            ]
            [
              (builtin sha2_256)
              [
                [ (builtin appendByteString) (con bytestring #67) ]
                (con bytestring #)
              ]
            ]
          ]
        ]
      ]
    ]
    [
      (force (force (delay (delay (lam x (lam y x))))))
      [
        (lam
          x0
          [
            (lam
              x2
              [
                [
                  (builtin multiplyInteger)
                  [
                    [ (builtin multiplyInteger) (con integer 3) ]
                    (con integer 1)
                  ]
                ]
                [
                  [ (builtin subtractInteger) (con integer 1) ] (con integer 3)
                ]
              ]
            )
            [
              [
                (builtin equalsByteString)
                [ (builtin sha3_256) (con bytestring #756c6b) ]
              ]
              [
                [ (builtin appendByteString) (con bytestring #) ]
                (con bytestring #66)
              ]
            ]
          ]
        )
        [
          (lam
            x1
            [
              [
                (builtin lessThanEqualsInteger)
                [ [ (builtin addInteger) (con integer 0) ] (con integer 0) ]
              ]
              [ [ (builtin subtractInteger) (con integer 2) ] (con integer 0) ]
            ]
          )
          [
            [
              (builtin equalsByteString)
              [
                [ (builtin appendByteString) (con bytestring #) ]
                (con bytestring #68)
              ]
            ]
            [ (builtin sha3_256) (con bytestring #6e7177) ]
          ]
        ]
      ]
    ]
  ]
)
