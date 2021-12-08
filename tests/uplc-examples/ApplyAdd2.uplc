(program
  1.0.0
  [
    [
      [
        (force (force (delay (delay (lam f (lam x [ f x ]))))))
        (builtin addInteger)
      ]
      [
        (lam
          x0
          [
            [ (builtin addInteger) (con integer 0) ]
            [
              [
                (builtin multiplyInteger)
                [ [ (builtin addInteger) (con integer 2) ] (con integer 3) ]
              ]
              [ [ (builtin multiplyInteger) (con integer 0) ] (con integer 2) ]
            ]
          ]
        )
        [
          (builtin sha2_256)
          [ (builtin sha2_256) [ (builtin sha3_256) (con bytestring #) ] ]
        ]
      ]
    ]
    [
      (lam
        x0
        [
          (lam
            x2
            [
              (lam
                x3 [ [ (builtin addInteger) (con integer 3) ] (con integer 3) ]
              )
              [ (builtin sha3_256) (con bytestring #7677) ]
            ]
          )
          [
            [
              (builtin lessThanEqualsInteger)
              [ [ (builtin multiplyInteger) (con integer 3) ] (con integer 1) ]
            ]
            [ [ (builtin multiplyInteger) (con integer 2) ] (con integer 1) ]
          ]
        ]
      )
      [
        (lam
          x1
          [
            (builtin sha2_256)
            [
              [ (builtin appendByteString) (con bytestring #7362) ]
              (con bytestring #6567)
            ]
          ]
        )
        [
          [
            (builtin multiplyInteger)
            [ [ (builtin multiplyInteger) (con integer 1) ] (con integer 0) ]
          ]
          [ [ (builtin addInteger) (con integer 1) ] (con integer 0) ]
        ]
      ]
    ]
  ]
)
