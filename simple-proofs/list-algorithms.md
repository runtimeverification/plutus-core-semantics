```k
requires "verification.k"

module LIST-ALGORITHMS
  imports VERIFICATION

  rule { M:Map [K <- V1] #Equals M [K <- V2] } => { V1 #Equals V2 } [simplification, anywhere]

  // --------------------------------------------------------

  // List-Free Invariant
  claim <k>
        < lam f_lstFree LIST_FREE_BODY RHO:Map > ~> 
        [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
        [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ _XS:ConstantList ] > ] ]
          =>
        < con list(integer) [ .ConstantList ] >
        ... </k>
        <env> .Map => .Map </env>
        requires RHO_1 ==K RHO [ f_0 <- < lam f_lstFree LIST_FREE_BODY RHO > ]
  [trusted]

  // 1. List-Free Algorithm
  claim <k>
    [ Z LIST_FREE ( con list(integer) [ _XS:ConstantList ] ) ] => 
        < con list(integer) [ .ConstantList ] >
        ... </k>
        <env> _ => .Map </env>

  // --------------------------------------------------------

  // List-Sum Invariant
  //  claim <k>
  //      < lam f_lstSum LIST_SUM_BODY RHO:Map > ~> 
  //      [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
  //      [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ XS:ConstantList ] > ] ]
  //        =>
  //      < con integer sum(XS) >
  //      ... </k>
  //      <env> .Map => .Map </env>
  //      requires RHO_1 ==K RHO [ f_0 <- < lam f_lstSum LIST_SUM_BODY RHO > ]
  //  [trusted]

  // 2. List-Sum Algorithm
  //claim <k>
  //  [ Z LIST_SUM ( con list(integer) [ _XS:ConstantList ] ) ] => 
  //      < con integer sum(XS) >
  //      ... </k>
  //      <env> _ => .Map </env>

  // --------------------------------------------------------

  // List-Length Invariant
  claim <k>
        < lam f_lstLen LIST_LEN_BODY RHO:Map > ~> 
        [_ ( lam y_0 [ x_0 x_0 y_0 ] ) RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] ] ~> 
        [_ y_0 RHO_1 [ x_0 <- < lam x_0 [ f_0 ( lam y_0 [ x_0 x_0 y_0 ] ) ] RHO_1 > ] [ y_0 <- < con list ( integer ) [ XS:ConstantList ] > ] ]
          =>
        < con integer length(XS) >
        ... </k>
        <env> .Map => .Map </env>
        requires RHO_1 ==K RHO [ f_0 <- < lam f_lstLen LIST_LEN_BODY RHO > ]
  [trusted]

  // 3. List-Length Algorithm
  claim <k>
    [ Z LIST_LEN ( con list(integer) [ XS:ConstantList ] ) ] => 
        < con integer length(XS) >
        ... </k>
        <env> _ => .Map </env>

endmodule
```

