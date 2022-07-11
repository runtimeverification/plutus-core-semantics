```k
requires "verification.k"

module LIST-ALGORITHMS
  imports VERIFICATION

  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification, anywhere]

  rule 
  {
    RHO:Map [ f_lstLonger <- < lam y_0 [ x_0 x_0 y_0 ] RHO:Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO:Map > ] [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO:Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO:Map > ] > ] > ] [ in_lst1 <- < con list ( integer ) [ XS1:ConstantList ] > ] [ in_lst2 <- < con list ( integer ) [ YS1:ConstantList ] > ]
  #Equals
    RHO0:Map [ f_lstLonger <- < lam y_0 [ x_0 x_0 y_0 ] RHO0:Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO0:Map > ] [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO0:Map [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO0:Map > ] > ] > ] [ in_lst1 <- < con list ( integer ) [ XS2:ConstantList ] > ] [ in_lst2 <- < con list ( integer ) [ YS2:ConstantList ] > ]
  } => 
  { 
    true #Equals 
            RHO ==K RHO0 
    andBool XS1 ==K XS2 
    andBool YS1 ==K YS2
  } [simplification, anywhere]

  // --------------------------------------------------------

  // List-Free Invariant
  claim 
    <k>
      < lam f_lstFree LIST_FREE_BODY RHO:Map > ~> 
      [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
      [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ _XS:ConstantList ] > ] ]
        =>
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env> .Map => .Map </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]

  // 1. List-Free Algorithm
  claim 
    <k>
      [ Z LIST_FREE ( con list(integer) [ _XS:ConstantList ] ) ] 
        => 
      < con list(integer) [ .ConstantList ] > ... 
    </k>
    <env> _ => .Map </env>

  // --------------------------------------------------------

  // List-Sum Invariant
  //claim <k>
  //  < lam f_lstSum LIST_SUM_BODY RHO:Map > ~> 
  //  [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
  //  [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ XS:ConstantList ] > ] ]
  //    =>
  //  < con integer sum(XS) >
  //  ... </k>
  //  <env> .Map => .Map </env>
  //  requires RHO_1 ==K RHO [ f_0 <- < lam f_lstSum LIST_SUM_BODY RHO > ]
  //   andBool allInts(XS)

  // 2. List-Sum Algorithm
  //claim <k>
  //  [ Z LIST_SUM ( con list(integer) [ XS:ConstantList ] ) ] 
  //    => 
  //  < con integer sum(XS) >
  //  ... </k>
  //  <env> _ => .Map </env>
  //  requires allInts(XS)

  // --------------------------------------------------------

  // List-Length Invariant
  claim 
    <k>
      < lam f_lstLen LIST_LEN_BODY RHO:Map > ~> 
      [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
      [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ XS:ConstantList ] > ] ]
        =>
      < con integer length(XS) > ... 
    </k>
    <env> .Map => .Map </env>
    requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]

  // 3a. List-Length Algorithm
  claim 
    <k>
      [ Z LIST_LEN (con list(integer) [ XS:ConstantList ]) ] 
        => 
      < con integer length(XS) > ... 
    </k>
    <env> _ => .Map </env>

  // --------------------------------------------------------

  // 3b. Simple List-length Client 
  claim 
    <k>
      [ TWO_LISTS_LEN_SUM (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con integer length(XS) +Int length(YS) > ... 
    </k>
    <env> _ => .Map </env>

  // --------------------------------------------------------

  // Longer-list Invariant - doesn't work
  claim 
  <k>
    LIST_LONGER_LOOP_BODY
      =>
    < con bool longerList(XS, YS) > ...
  </k>
  <env> RHO:Map 
          [ f_lstLonger <- < lam y_0 [ x_0 x_0 y_0 ] RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] > ] 
          [ in_lst1 <- < con list ( integer ) [ XS:ConstantList ] > ] 
          [ in_lst2 <- < con list ( integer ) [ YS:ConstantList ] > ] => .Map </env>
  requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLonger LIST_LONGER_BODY RHO > ]

  // 3c. Longer list
  claim 
    <k>
      [ Z LIST_LONGER (con list(integer) [ XS:ConstantList ]) (con list(integer) [ YS:ConstantList ]) ]
        =>
      < con bool longerList(XS, YS) >
    ... </k>
    <env> _ => .Map </env>

endmodule
```