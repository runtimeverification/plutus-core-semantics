(program 1.0.0
  [ (lam f [(lam x [f (lam y [x x y])]) (lam x [f (lam y [x x y])])])
    (lam fct
      (lam n (force [ (force (builtin ifThenElse))
                      [ (builtin equalsInteger) (con integer 0) n ]
             (delay     (con integer 1))
             (delay   [ (builtin multiplyInteger) n [ fct [ (builtin subtractInteger) n (con integer 1) ] ] ])
                    ]
             )
      )
    )
    (con integer 10)
  ]
)
