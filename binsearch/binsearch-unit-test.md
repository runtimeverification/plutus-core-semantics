```k
requires "verification.md"

module BINSEARCH-UNIT-TEST
  imports VERIFICATION
    claim <k> [ BINSEARCH_REC
                (con list(integer)
                  [ 1 ,
                    [ 2 , [ .ConstantList ] , [ .ConstantList ] ],
                    [ 3 , [ .ConstantList ] , [ .ConstantList ] ],
                    .ConstantList
                  ]
                 )
               (con integer 1) ]
          => (con bool True) ...
          </k>
    <env> .Map </env>

    claim <k>
      [ ( force ( builtin tailList ) )
        ( con list ( integer )
          [ 1 ,
            [ 4 ,
              [ 5 , [ .ConstantList ], [ .ConstantList ] ],
              [ 6 , [ .ConstantList ], [ .ConstantList ] ],
              .ConstantList
            ],
            [ 3 , [ .ConstantList ], [ .ConstantList ] ],
            .ConstantList
          ]
        )
      ]
    =>
     < con list(integer)
       [
         [ 4 ,
           [ 5 , [ .ConstantList ] , [ .ConstantList ] ] ,
           [ 6 , [ .ConstantList ] , [ .ConstantList ] ] ,
           .ConstantList
         ] ,
         [ 3 , [ .ConstantList ] , [ .ConstantList ] ] ,
         .ConstantList
       ]
     > ...
  </k>
  <env> .Map </env>

  claim <k>
      [ ( force ( builtin headList ) )
        ( con list ( integer )
          [ 1 ,
            [ 4 ,
              [ 5 , [ .ConstantList ], [ .ConstantList ] ],
              [ 6 , [ .ConstantList ], [ .ConstantList ] ],
              .ConstantList
            ],
            [ 3 , [ .ConstantList ], [ .ConstantList ] ],
            .ConstantList
          ]
        )
      ]
    =>
     < con integer 1 >  ...
  </k>
  <env> .Map </env>

  claim <k>
      [ ( force ( builtin tailList ) )
        ( con list ( integer )
          [ _I:Int , CL:ConstantList ]
        )
      ]
    =>
     < con list(integer) [ CL ] > ...
  </k>
  <env> .Map </env>

  claim <k>
      [ ( force ( builtin headList ) )
        ( con list ( integer )
          [ I:Int , _CL:ConstantList ]
        )
      ]
    =>
     < con integer I > ...
  </k>
  <env> .Map </env>
endmodule
```