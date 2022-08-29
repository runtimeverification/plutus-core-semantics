(program 1.0.0
  [ (lam maxList [ maxList (con list(integer) [ 1, 2, 24, 6, 3, 12, 3, 18, 100, 4 ] ) ])
    [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
      (lam max (lam l
        [ (lam lhead (lam ltail
            (force [ (force (force (builtin chooseList))) ltail
              (delay lhead)
              (delay [ (lam hd (lam maxtail
                       [ (force (builtin ifThenElse))
                         [ (builtin lessThanInteger) hd maxtail ]
                         maxtail
                         hd
                       ]))
                       [ (force (builtin headList)) ltail ]
                       [ max ltail ]
                     ]
              )
            ])

          ))
          [ (force (builtin headList)) l ]
          [ (force (builtin tailList)) l ]
        ]
      ))
    ]
  ]
)
