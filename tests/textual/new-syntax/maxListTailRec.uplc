(program 1.0.0
  [ (lam maxList [ maxList (con list(integer) [ 1, 2, 24, 6, 3, 12, 3, 18, 100, 4 ] ) ])
    [ (lam maxListAux (lam l [ maxListAux [ (force (builtin tailList)) l ] [ (force (builtin headList)) l ] ]))
      [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
        (lam max (lam l (lam acc
          (force [ (force (force (builtin chooseList)))
                   l
          (delay   acc)
          (delay   [ max
                   [ (force (builtin tailList)) l ]
                   [ (force (builtin ifThenElse))
                     [ (builtin lessThanInteger) acc [ (force (builtin headList)) l ] ]
                     [ (force (builtin headList)) l ]
                     acc
                   ]])
          ])
        )))
      ]
    ]
  ]
)
