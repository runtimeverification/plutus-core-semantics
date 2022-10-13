(program 1.0.0
  [
    (lam zero
      [
        (lam number5
          [
            (lam add5
              [
                (lam number10
                  [
                    (lam equals10
                      [ equals10 [ add5 [ add5 zero ] ] ]
                    )
                    [ (builtin equalsInteger) number10 ]
                  ]
                )
                (con integer 10)
              ]
            )
            [ (builtin addInteger) number5 ]
          ]
        )
        (con integer 5)
      ]
    )
    (con integer 0)
  ]
)
