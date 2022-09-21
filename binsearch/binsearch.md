```k
requires "uplc.md"

module REC
  imports UPLC-SYNTAX
  syntax Term ::= "REC" [alias]
  rule REC => (lam f_0
                [ (lam w_0 [ w_0 w_0 ])
                  (lam w_0 (lam x_0 [ [ f_0 [ w_0 w_0 ] ] x_0 ])) ])
endmodule

module BINSEARCH
  imports REC
  imports UPLC-SYNTAX

  syntax Term ::= "BINSEARCH" [alias]
  syntax Term ::= "BINSEARCH_REC" [alias]
  syntax Term ::= "BINSEARCH_BODY" [alias]
  syntax Term ::= "BINSEARCH_LOOP" [alias]
  syntax Term ::= "HEAD" [alias]
  syntax Term ::= "TAIL" [alias]
  syntax Term ::= "LEFT_TREE" [alias]
  syntax Term ::= "RIGHT_TREE" [alias]

  rule BINSEARCH_REC => [ REC BINSEARCH ]

  rule BINSEARCH => (lam bin_search BINSEARCH_BODY)

  rule BINSEARCH_BODY => (lam in_list (lam in_e BINSEARCH_LOOP))

  rule HEAD => (force (builtin headList))

  rule TAIL => (force (builtin tailList))

  rule LEFT_TREE => [ HEAD [ TAIL in_list ] ]

  rule RIGHT_TREE => [ HEAD [ TAIL [ TAIL in_list ] ] ]

  rule BINSEARCH_LOOP =>
    (force
      [ (force (builtin ifThenElse))
        [ (force (builtin nullList)) in_list ]
        ( delay (con bool False) )
        ( delay
          (force
            [ (force (builtin ifThenElse))
              [ (force (builtin equalsInteger)) in_e [ HEAD in_list ] ]
              (delay (con bool True))
              [ (force (builtin ifThenElse))
                [ (force (builtin lessThanInteger)) in_e HEAD ]
                (delay [ bin_search LEFT_TREE in_e ] )
                (delay [ bin_search RIGHT_TREE in_e ] )
              ]
            ]
          )
        )
      ]
    )
endmodule
```
