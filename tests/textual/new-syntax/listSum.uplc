(program 1.0.0

  [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
    (lam lstAdd
      (lam lst (force [ (force (builtin ifThenElse))
               [ (force (builtin nullList)) lst ]

               ( delay (con integer 0) )

               ( delay
                 [ (builtin addInteger)
                   [ (force (builtin headList)) lst ]
                   [ lstAdd [ (force (builtin tailList)) lst ] ]
                 ]
               )
             ])
      )
    )
    (con list(integer) [ 5 , 6 ] )
  ]
)
