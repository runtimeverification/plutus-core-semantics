```k
requires "verification.k"

module LIST-ALGORITHMS
  imports VERIFICATION

  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification, anywhere]

  rule 
  {
    M1:Map [ K1 <- V1  ] [ K2 <- V2  ] #Equals
    M2:Map [ K1 <- V1' ] [ K2 <- V2' ] 
  } => 
  { 
    true #Equals 
            V1 ==K V1' andBool V2 ==K V2'
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ]
  } [simplification, anywhere]

  rule 
  {
    M1:Map [ K1 <- V1  ] [ K2 <- V2  ] [ K3 <- V3  ]
  #Equals
    M2:Map [ K1 <- V1' ] [ K2 <- V2' ] [ K3 <- V3' ]
  } => 
  { 
    true #Equals 
            V1 ==K V1' andBool V2 ==K V2' andBool V3 ==K V3'
    andBool M1 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ] ==K M2 [ K1 <- undef ] [ K2 <- undef ] [ K3 <- undef ]
  } [simplification, anywhere]

  // --------------------------------------------------------

  // 1. List-Free Algorithm
  claim 
    <k>
      [ Z LIST_FREE ( con list(integer) [ _XS:ConstantList ] ) ] 
        => 
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env> _ => .Map </env>

  // List-Free Invariant
  claim 
    <k>
      LIST_FREE_LOOP_BODY
        =>
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstFree <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ _XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]

  // --------------------------------------------------------

  // 2. List-Sum Algorithm
  claim <k>
    [ Z LIST_SUM ( con list(integer) [ XS:ConstantList ] ) ] 
      => 
    < con integer sum(XS) >
    ... </k>
    <env> _ => .Map </env>
    requires allInts(XS)

  // List-Sum Invariant
  claim 
    <k>
      LIST_SUM_LOOP_BODY
        =>
      < con integer sum(XS) > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstSum <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstSum LIST_SUM_BODY RHO > ]
     andBool allInts(XS)

  // --------------------------------------------------------

  // 3. List-Length Algorithm
  claim 
    <k>
      [ Z LIST_LEN (con list(integer) [ XS:ConstantList ]) ] 
        => 
      < con integer length(XS) > ... 
    </k>
    <env> _ => .Map </env>

  // List-Length Invariant
  claim 
    <k>
      LIST_LENGTH_LOOP_BODY
        =>
      < con integer length(XS) > ... 
    </k>
    <env>
      RHO:Map 
        [ f_lstLen <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
        [ in_lst <- < con list ( integer ) [ XS:ConstantList ] > ] => .Map 
    </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]

  // --------------------------------------------------------

  // 4. Simple List-length Client 
  claim 
    <k>
      [ TWO_LISTS_LEN_SUM (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con integer length(XS) +Int length(YS) > ... 
    </k>
    <env> _ => .Map </env>

  // --------------------------------------------------------

  // 5. List-longer Algorithm
  claim 
    <k>
      [ Z LIST_LONGER (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con bool longerList(XS, YS) >
    ... </k>
    <env> _ => .Map </env>

  // List-longer Invariant
  claim 
  <k>
    LIST_LONGER_LOOP_BODY
      =>
    < con bool longerList(XS, YS) > ...
  </k>
  <env> RHO:Map 
          [ f_lstLonger <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
          [ in_lst1 <- < con list ( integer ) [ XS:ConstantList ] > ] 
          [ in_lst2 <- < con list ( integer ) [ YS:ConstantList ] > ] => .Map 
  </env>
  requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO > ]

endmodule
```