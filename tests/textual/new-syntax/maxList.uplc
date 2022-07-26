(program 1.0.0
  [ (lam maxList [ maxList (con list(integer) [ 1, 2, 24, 6, 3, 12, 3, 18, 100, 4 ] ) ])
    [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
      (lam max (lam l
        (force [ (force (force (builtin chooseList))) l
          (delay (con integer 0))
          (delay [ (lam hd (lam maxtail
                   [ (force (builtin ifThenElse))
                     [ (builtin lessThanInteger) hd maxtail ]
                     maxtail
                     hd
                   ]))
                   [ (force (builtin headList)) l ]
                   [ max [ (force (builtin tailList)) l ] ]
                 ]
          )
        ])
      ))
    ]
  ]
)
