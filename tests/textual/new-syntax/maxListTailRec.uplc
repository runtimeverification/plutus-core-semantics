(program 1.0.0
  [ (lam maxList [ maxList (con list(integer) [ 1, 2, 24, 6, 3, 12, 3, 18, 100, 4 ] ) ])
    [ (lam maxListAux (lam l [ maxListAux l (con integer 0) ]))
      [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
        (lam max (lam l (lam cont
          (force [ (force (builtin chooseList))
                   l
          (delay   cont)
          (delay   [ max
                   [ (force (builtin tailList)) l ]
                   [ (force (builtin ifThenElse))
                     [ (builtin lessThanInteger) cont [ (force (builtin headList)) l ] ]
                     [ (force (builtin headList)) l ]
                     cont
                   ]])
          ])
        )))
      ]
    ]
  ]
)
