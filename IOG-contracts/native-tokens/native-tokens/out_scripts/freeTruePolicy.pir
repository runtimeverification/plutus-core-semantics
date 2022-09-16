(program
  (let
    (nonrec)
    (termbind
      (strict)
      (vardecl reconstructCaseError_1955 (con string))
      (con string "PT1")
    )
    (datatypebind
      (datatype
        (tyvardecl Unit_1829 (type))

        Unit_match_1831
        (vardecl Unit_1830 Unit_1829)
      )
    )
    (termbind
      (strict)
      (vardecl error_1825 (all a_1826 (type) (fun (con unit) a_1826)))
      (abs a_1823 (type) (lam thunk_1824 (con unit) (error a_1823)))
    )
    (termbind
      (strict)
      (vardecl
        trace_1821 (all a_1822 (type) (fun (con string) (fun a_1822 a_1822)))
      )
      (builtin trace)
    )
    (termbind (strict) (vardecl unitval_1820 (con unit)) (con unit ()))
    (termbind
      (nonstrict)
      (vardecl x_2415 Unit_1829)
      [
        { error_1825 Unit_1829 }
        [
          {
            [
              Unit_match_1831
              [
                [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ] Unit_1830
              ]
            ]
            (con unit)
          }
          unitval_1820
        ]
      ]
    )
    (termbind
      (strict)
      (vardecl
        equalsInteger_1963 (fun (con integer) (fun (con integer) (con bool)))
      )
      (builtin equalsInteger)
    )
    (termbind
      (strict)
      (vardecl
        fst_1960
        (all
          a_1961
          (type)
          (all b_1962 (type) (fun [ [ (con pair) a_1961 ] b_1962 ] a_1961))
        )
      )
      (builtin fstPair)
    )
    (termbind
      (strict)
      (vardecl
        ifThenElse_1956
        (all a_1957 (type) (fun (con bool) (fun a_1957 (fun a_1957 a_1957))))
      )
      (builtin ifThenElse)
    )
    (termbind
      (strict)
      (vardecl
        unsafeDataAsConstr_1949
        (fun
          (con data) [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
        )
      )
      (builtin unConstrData)
    )
    (termbind
      (strict)
      (vardecl
        fUnsafeFromDataUnit_cunsafeFromBuiltinData_2417
        (fun (con data) Unit_1829)
      )
      (lam
        d_2412
        (con data)
        [
          [
            [
              [
                { ifThenElse_1956 (fun (con unit) Unit_1829) }
                [
                  [
                    equalsInteger_1963
                    [
                      { { fst_1960 (con integer) } [ (con list) (con data) ] }
                      [ unsafeDataAsConstr_1949 d_2412 ]
                    ]
                  ]
                  (con integer 0)
                ]
              ]
              (lam ds_2413 (con unit) Unit_1830)
            ]
            (lam ds_2416 (con unit) x_2415)
          ]
          unitval_1820
        ]
      )
    )
    (termbind
      (nonstrict)
      (vardecl
        fUnsafeFromDataUnit_2418
        [ (lam a_2419 (type) (fun (con data) a_2419)) Unit_1829 ]
      )
      fUnsafeFromDataUnit_cunsafeFromBuiltinData_2417
    )
    (datatypebind
      (datatype
        (tyvardecl Credential_1871 (type))

        Credential_match_1874
        (vardecl PubKeyCredential_1872 (fun (con bytestring) Credential_1871))
        (vardecl ScriptCredential_1873 (fun (con bytestring) Credential_1871))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl StakingCredential_1875 (type))

        StakingCredential_match_1878
        (vardecl StakingHash_1876 (fun Credential_1871 StakingCredential_1875))
        (vardecl
          StakingPtr_1877
          (fun
            (con integer)
            (fun (con integer) (fun (con integer) StakingCredential_1875))
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl DCert_1920 (type))

        DCert_match_1928
        (vardecl
          DCertDelegDeRegKey_1921 (fun StakingCredential_1875 DCert_1920)
        )
        (vardecl
          DCertDelegDelegate_1922
          (fun StakingCredential_1875 (fun (con bytestring) DCert_1920))
        )
        (vardecl DCertDelegRegKey_1923 (fun StakingCredential_1875 DCert_1920))
        (vardecl DCertGenesis_1924 DCert_1920)
        (vardecl DCertMir_1925 DCert_1920)
        (vardecl
          DCertPoolRegister_1926
          (fun (con bytestring) (fun (con bytestring) DCert_1920))
        )
        (vardecl
          DCertPoolRetire_1927
          (fun (con bytestring) (fun (con integer) DCert_1920))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef_1890 (type))

        TxOutRef_match_1892
        (vardecl
          TxOutRef_1891 (fun (con bytestring) (fun (con integer) TxOutRef_1890))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl ScriptPurpose_1940 (type))

        ScriptPurpose_match_1945
        (vardecl Certifying_1941 (fun DCert_1920 ScriptPurpose_1940))
        (vardecl Minting_1942 (fun (con bytestring) ScriptPurpose_1940))
        (vardecl Rewarding_1943 (fun StakingCredential_1875 ScriptPurpose_1940))
        (vardecl Spending_1944 (fun TxOutRef_1890 ScriptPurpose_1940))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool_1903 (type))

        Bool_match_1906
        (vardecl True_1904 Bool_1903) (vardecl False_1905 Bool_1903)
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Extended_1897 (fun (type) (type)))
        (tyvardecl a_1902 (type))
        Extended_match_1901
        (vardecl Finite_1898 (fun a_1902 [ Extended_1897 a_1902 ]))
        (vardecl NegInf_1899 [ Extended_1897 a_1902 ])
        (vardecl PosInf_1900 [ Extended_1897 a_1902 ])
      )
    )
    (datatypebind
      (datatype
        (tyvardecl LowerBound_1911 (fun (type) (type)))
        (tyvardecl a_1914 (type))
        LowerBound_match_1913
        (vardecl
          LowerBound_1912
          (fun
            [ Extended_1897 a_1914 ] (fun Bool_1903 [ LowerBound_1911 a_1914 ])
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl UpperBound_1907 (fun (type) (type)))
        (tyvardecl a_1910 (type))
        UpperBound_match_1909
        (vardecl
          UpperBound_1908
          (fun
            [ Extended_1897 a_1910 ] (fun Bool_1903 [ UpperBound_1907 a_1910 ])
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Interval_1915 (fun (type) (type)))
        (tyvardecl a_1918 (type))
        Interval_match_1917
        (vardecl
          Interval_1916
          (fun
            [ LowerBound_1911 a_1918 ]
            (fun [ UpperBound_1907 a_1918 ] [ Interval_1915 a_1918 ])
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Maybe_1863 (fun (type) (type)))
        (tyvardecl a_1867 (type))
        Maybe_match_1866
        (vardecl Just_1864 (fun a_1867 [ Maybe_1863 a_1867 ]))
        (vardecl Nothing_1865 [ Maybe_1863 a_1867 ])
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Address_1879 (type))

        Address_match_1881
        (vardecl
          Address_1880
          (fun
            Credential_1871
            (fun [ Maybe_1863 StakingCredential_1875 ] Address_1879)
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Tuple2_1848 (fun (type) (fun (type) (type))))
        (tyvardecl a_1851 (type)) (tyvardecl b_1852 (type))
        Tuple2_match_1850
        (vardecl
          Tuple2_1849
          (fun a_1851 (fun b_1852 [ [ Tuple2_1848 a_1851 ] b_1852 ]))
        )
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List_1841 (fun (type) (type)))
          (tyvardecl a_1845 (type))
          Nil_match_1844
          (vardecl Nil_1842 [ List_1841 a_1845 ])
          (vardecl
            Cons_1843
            (fun a_1845 (fun [ List_1841 a_1845 ] [ List_1841 a_1845 ]))
          )
        )
      )
      (let
        (nonrec)
        (datatypebind
          (datatype
            (tyvardecl TxOut_1882 (type))

            TxOut_match_1884
            (vardecl
              TxOut_1883
              (fun
                Address_1879
                (fun
                  [
                    [
                      (lam
                        k_1885
                        (type)
                        (lam
                          v_1886
                          (type)
                          [ List_1841 [ [ Tuple2_1848 k_1885 ] v_1886 ] ]
                        )
                      )
                      (con bytestring)
                    ]
                    [
                      [
                        (lam
                          k_1887
                          (type)
                          (lam
                            v_1888
                            (type)
                            [ List_1841 [ [ Tuple2_1848 k_1887 ] v_1888 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      (con integer)
                    ]
                  ]
                  (fun [ Maybe_1863 (con bytestring) ] TxOut_1882)
                )
              )
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInInfo_1893 (type))

            TxInInfo_match_1895
            (vardecl
              TxInInfo_1894 (fun TxOutRef_1890 (fun TxOut_1882 TxInInfo_1893))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInfo_1929 (type))

            TxInfo_match_1931
            (vardecl
              TxInfo_1930
              (fun
                [ List_1841 TxInInfo_1893 ]
                (fun
                  [ List_1841 TxOut_1882 ]
                  (fun
                    [
                      [
                        (lam
                          k_1932
                          (type)
                          (lam
                            v_1933
                            (type)
                            [ List_1841 [ [ Tuple2_1848 k_1932 ] v_1933 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      [
                        [
                          (lam
                            k_1934
                            (type)
                            (lam
                              v_1935
                              (type)
                              [ List_1841 [ [ Tuple2_1848 k_1934 ] v_1935 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        (con integer)
                      ]
                    ]
                    (fun
                      [
                        [
                          (lam
                            k_1936
                            (type)
                            (lam
                              v_1937
                              (type)
                              [ List_1841 [ [ Tuple2_1848 k_1936 ] v_1937 ] ]
                            )
                          )
                          (con bytestring)
                        ]
                        [
                          [
                            (lam
                              k_1938
                              (type)
                              (lam
                                v_1939
                                (type)
                                [ List_1841 [ [ Tuple2_1848 k_1938 ] v_1939 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      ]
                      (fun
                        [ List_1841 DCert_1920 ]
                        (fun
                          [
                            List_1841
                            [
                              [ Tuple2_1848 StakingCredential_1875 ]
                              (con integer)
                            ]
                          ]
                          (fun
                            [ Interval_1915 (con integer) ]
                            (fun
                              [ List_1841 (con bytestring) ]
                              (fun
                                [
                                  List_1841
                                  [
                                    [ Tuple2_1848 (con bytestring) ] (con data)
                                  ]
                                ]
                                (fun (con bytestring) TxInfo_1929)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ScriptContext_1946 (type))

            ScriptContext_match_1948
            (vardecl
              ScriptContext_1947
              (fun TxInfo_1929 (fun ScriptPurpose_1940 ScriptContext_1946))
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            mkPolicyTrue_2376 (fun Unit_1829 (fun ScriptContext_1946 Bool_1903))
          )
          (lam
            ds_2373
            Unit_1829
            (lam
              ds_2374
              ScriptContext_1946
              [ { [ Unit_match_1831 ds_2373 ] Bool_1903 } True_1904 ]
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl x_2344 ScriptContext_1946)
          [
            { error_1825 ScriptContext_1946 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2327 ScriptPurpose_1940)
          [
            { error_1825 ScriptPurpose_1940 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2234 DCert_1920)
          [
            { error_1825 DCert_1920 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2063 StakingCredential_1875)
          [
            { error_1825 StakingCredential_1875 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2044 Credential_1871)
          [
            { error_1825 Credential_1871 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (strict)
          (vardecl unsafeDataAsB_1967 (fun (con data) (con bytestring)))
          (builtin unBData)
        )
        (termbind
          (strict)
          (vardecl
            head_1958 (all a_1959 (type) (fun [ (con list) a_1959 ] a_1959))
          )
          (builtin headList)
        )
        (termbind
          (strict)
          (vardecl
            snd_1952
            (all
              a_1953
              (type)
              (all b_1954 (type) (fun [ [ (con pair) a_1953 ] b_1954 ] b_1954))
            )
          )
          (builtin sndPair)
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataCredential_cunsafeFromBuiltinData_2048
            (fun (con data) Credential_1871)
          )
          (lam
            d_2034
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2035
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2034 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2040 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2035
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2041 Credential_1871)
                [ PubKeyCredential_1872 x_2040 ]
              )
              (termbind
                (nonstrict)
                (vardecl index_2036 (con integer))
                [
                  { { fst_1960 (con integer) } [ (con list) (con data) ] }
                  tup_2035
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2046 Credential_1871)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) Credential_1871) }
                        [ [ equalsInteger_1963 index_2036 ] (con integer 0) ]
                      ]
                      (lam ds_2042 (con unit) x_2041)
                    ]
                    (lam ds_2045 (con unit) x_2044)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2037 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2035
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2038 Credential_1871)
                [ ScriptCredential_1873 x_2037 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) Credential_1871) }
                      [ [ equalsInteger_1963 index_2036 ] (con integer 1) ]
                    ]
                    (lam ds_2039 (con unit) x_2038)
                  ]
                  (lam ds_2047 (con unit) x_2046)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl unsafeDataAsI_1966 (fun (con data) (con integer)))
          (builtin unIData)
        )
        (termbind
          (strict)
          (vardecl
            tail_1950
            (all
              a_1951 (type) (fun [ (con list) a_1951 ] [ (con list) a_1951 ])
            )
          )
          (builtin tailList)
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
            (fun (con data) StakingCredential_1875)
          )
          (lam
            d_2049
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2050
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2049 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2059 Credential_1871)
                [
                  fUnsafeFromDataCredential_cunsafeFromBuiltinData_2048
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2050
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2060 StakingCredential_1875)
                [ StakingHash_1876 x_2059 ]
              )
              (termbind
                (nonstrict)
                (vardecl index_2051 (con integer))
                [
                  { { fst_1960 (con integer) } [ (con list) (con data) ] }
                  tup_2050
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2065 StakingCredential_1875)
                [
                  [
                    [
                      [
                        {
                          ifThenElse_1956
                          (fun (con unit) StakingCredential_1875)
                        }
                        [ [ equalsInteger_1963 index_2051 ] (con integer 0) ]
                      ]
                      (lam ds_2061 (con unit) x_2060)
                    ]
                    (lam ds_2064 (con unit) x_2063)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2052 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2050
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2053 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2052 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2056 (con integer))
                [
                  unsafeDataAsI_1966
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2053 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2055 (con integer))
                [ unsafeDataAsI_1966 [ { head_1958 (con data) } t_2053 ] ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2054 (con integer))
                [ unsafeDataAsI_1966 [ { head_1958 (con data) } t_2052 ] ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2057 StakingCredential_1875)
                [ [ [ StakingPtr_1877 x_2054 ] x_2055 ] x_2056 ]
              )
              [
                [
                  [
                    [
                      {
                        ifThenElse_1956 (fun (con unit) StakingCredential_1875)
                      }
                      [ [ equalsInteger_1963 index_2051 ] (con integer 1) ]
                    ]
                    (lam ds_2058 (con unit) x_2057)
                  ]
                  (lam ds_2066 (con unit) x_2065)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataDCert_cunsafeFromBuiltinData_2248
            (fun (con data) DCert_1920)
          )
          (lam
            d_2207
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2208
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2207 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2230 StakingCredential_1875)
                [
                  fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2208
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2231 DCert_1920)
                [ DCertDelegRegKey_1923 x_2230 ]
              )
              (termbind
                (nonstrict)
                (vardecl index_2209 (con integer))
                [
                  { { fst_1960 (con integer) } [ (con list) (con data) ] }
                  tup_2208
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2236 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 0) ]
                      ]
                      (lam ds_2232 (con unit) x_2231)
                    ]
                    (lam ds_2235 (con unit) x_2234)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2227 StakingCredential_1875)
                [
                  fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2208
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2228 DCert_1920)
                [ DCertDelegDeRegKey_1921 x_2227 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2238 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 1) ]
                      ]
                      (lam ds_2229 (con unit) x_2228)
                    ]
                    (lam ds_2237 (con unit) x_2236)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2222 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2208
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2224 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2222 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2223 StakingCredential_1875)
                [
                  fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                  [ { head_1958 (con data) } t_2222 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2225 DCert_1920)
                [ [ DCertDelegDelegate_1922 x_2223 ] x_2224 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2240 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 2) ]
                      ]
                      (lam ds_2226 (con unit) x_2225)
                    ]
                    (lam ds_2239 (con unit) x_2238)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2217 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2208
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2219 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2217 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2218 (con bytestring))
                [ unsafeDataAsB_1967 [ { head_1958 (con data) } t_2217 ] ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2220 DCert_1920)
                [ [ DCertPoolRegister_1926 x_2218 ] x_2219 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2242 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 3) ]
                      ]
                      (lam ds_2221 (con unit) x_2220)
                    ]
                    (lam ds_2241 (con unit) x_2240)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2212 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2208
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2214 (con integer))
                [
                  unsafeDataAsI_1966
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2212 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2213 (con bytestring))
                [ unsafeDataAsB_1967 [ { head_1958 (con data) } t_2212 ] ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2215 DCert_1920)
                [ [ DCertPoolRetire_1927 x_2213 ] x_2214 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2244 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 4) ]
                      ]
                      (lam ds_2216 (con unit) x_2215)
                    ]
                    (lam ds_2243 (con unit) x_2242)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2246 DCert_1920)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) DCert_1920) }
                        [ [ equalsInteger_1963 index_2209 ] (con integer 5) ]
                      ]
                      (lam ds_2211 (con unit) DCertGenesis_1924)
                    ]
                    (lam ds_2245 (con unit) x_2244)
                  ]
                  unitval_1820
                ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) DCert_1920) }
                      [ [ equalsInteger_1963 index_2209 ] (con integer 6) ]
                    ]
                    (lam ds_2210 (con unit) DCertMir_1925)
                  ]
                  (lam ds_2247 (con unit) x_2246)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl x_2114 TxOutRef_1890)
          [
            { error_1825 TxOutRef_1890 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2103 (con bytestring))
          [
            { error_1825 (con bytestring) }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataTxId_cunsafeFromBuiltinData_2105
            (fun (con data) (con bytestring))
          )
          (lam
            d_2098
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2099
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2098 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2100 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2099
                    ]
                  ]
                ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) (con bytestring)) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2099
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2101 (con unit) x_2100)
                  ]
                  (lam ds_2104 (con unit) x_2103)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2116
            (fun (con data) TxOutRef_1890)
          )
          (lam
            d_2106
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2107
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2106 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2108 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2107
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2110 (con integer))
                [
                  unsafeDataAsI_1966
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2108 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2109 (con bytestring))
                [
                  fUnsafeFromDataTxId_cunsafeFromBuiltinData_2105
                  [ { head_1958 (con data) } t_2108 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2111 TxOutRef_1890)
                [ [ TxOutRef_1891 x_2109 ] x_2110 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) TxOutRef_1890) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2107
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2112 (con unit) x_2111)
                  ]
                  (lam ds_2115 (con unit) x_2114)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2335
            (fun (con data) ScriptPurpose_1940)
          )
          (lam
            d_2311
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2312
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2311 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2323 (con bytestring))
                [
                  unsafeDataAsB_1967
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2312
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2324 ScriptPurpose_1940)
                [ Minting_1942 x_2323 ]
              )
              (termbind
                (nonstrict)
                (vardecl index_2313 (con integer))
                [
                  { { fst_1960 (con integer) } [ (con list) (con data) ] }
                  tup_2312
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2329 ScriptPurpose_1940)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) ScriptPurpose_1940) }
                        [ [ equalsInteger_1963 index_2313 ] (con integer 0) ]
                      ]
                      (lam ds_2325 (con unit) x_2324)
                    ]
                    (lam ds_2328 (con unit) x_2327)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2320 TxOutRef_1890)
                [
                  fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2116
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2312
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2321 ScriptPurpose_1940)
                [ Spending_1944 x_2320 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2331 ScriptPurpose_1940)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) ScriptPurpose_1940) }
                        [ [ equalsInteger_1963 index_2313 ] (con integer 1) ]
                      ]
                      (lam ds_2322 (con unit) x_2321)
                    ]
                    (lam ds_2330 (con unit) x_2329)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2317 StakingCredential_1875)
                [
                  fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2312
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2318 ScriptPurpose_1940)
                [ Rewarding_1943 x_2317 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2333 ScriptPurpose_1940)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) ScriptPurpose_1940) }
                        [ [ equalsInteger_1963 index_2313 ] (con integer 2) ]
                      ]
                      (lam ds_2319 (con unit) x_2318)
                    ]
                    (lam ds_2332 (con unit) x_2331)
                  ]
                  unitval_1820
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2314 DCert_1920)
                [
                  fUnsafeFromDataDCert_cunsafeFromBuiltinData_2248
                  [
                    { head_1958 (con data) }
                    [
                      { { snd_1952 (con integer) } [ (con list) (con data) ] }
                      tup_2312
                    ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2315 ScriptPurpose_1940)
                [ Certifying_1941 x_2314 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) ScriptPurpose_1940) }
                      [ [ equalsInteger_1963 index_2313 ] (con integer 3) ]
                    ]
                    (lam ds_2316 (con unit) x_2315)
                  ]
                  (lam ds_2334 (con unit) x_2333)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl x_2308 TxInfo_1929)
          [
            { error_1825 TxInfo_1929 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2267
            (all
              a_2268
              (type)
              (all
                b_2269
                (type)
                (fun
                  [ (lam a_2270 (type) (fun (con data) a_2270)) a_2268 ]
                  (fun
                    [ (lam a_2271 (type) (fun (con data) a_2271)) b_2269 ]
                    (fun (con data) [ [ Tuple2_1848 a_2268 ] b_2269 ])
                  )
                )
              )
            )
          )
          (abs
            a_2251
            (type)
            (abs
              b_2252
              (type)
              (let
                (nonrec)
                (termbind
                  (nonstrict)
                  (vardecl x_2265 [ [ Tuple2_1848 a_2251 ] b_2252 ])
                  [
                    { error_1825 [ [ Tuple2_1848 a_2251 ] b_2252 ] }
                    [
                      {
                        [
                          Unit_match_1831
                          [
                            [
                              { trace_1821 Unit_1829 } reconstructCaseError_1955
                            ]
                            Unit_1830
                          ]
                        ]
                        (con unit)
                      }
                      unitval_1820
                    ]
                  ]
                )
                (lam
                  dUnsafeFromData_2253
                  [ (lam a_2254 (type) (fun (con data) a_2254)) a_2251 ]
                  (lam
                    dUnsafeFromData_2255
                    [ (lam a_2256 (type) (fun (con data) a_2256)) b_2252 ]
                    (lam
                      d_2257
                      (con data)
                      (let
                        (nonrec)
                        (termbind
                          (nonstrict)
                          (vardecl
                            tup_2258
                            [
                              [ (con pair) (con integer) ]
                              [ (con list) (con data) ]
                            ]
                          )
                          [ unsafeDataAsConstr_1949 d_2257 ]
                        )
                        (termbind
                          (nonstrict)
                          (vardecl t_2259 [ (con list) (con data) ])
                          [
                            {
                              { snd_1952 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2258
                          ]
                        )
                        (termbind
                          (nonstrict)
                          (vardecl x_2261 b_2252)
                          [
                            dUnsafeFromData_2255
                            [
                              { head_1958 (con data) }
                              [ { tail_1950 (con data) } t_2259 ]
                            ]
                          ]
                        )
                        (termbind
                          (nonstrict)
                          (vardecl x_2260 a_2251)
                          [
                            dUnsafeFromData_2253
                            [ { head_1958 (con data) } t_2259 ]
                          ]
                        )
                        (termbind
                          (nonstrict)
                          (vardecl x_2262 [ [ Tuple2_1848 a_2251 ] b_2252 ])
                          [
                            [ { { Tuple2_1849 a_2251 } b_2252 } x_2260 ] x_2261
                          ]
                        )
                        [
                          [
                            [
                              [
                                {
                                  ifThenElse_1956
                                  (fun
                                    (con unit) [ [ Tuple2_1848 a_2251 ] b_2252 ]
                                  )
                                }
                                [
                                  [
                                    equalsInteger_1963
                                    [
                                      {
                                        { fst_1960 (con integer) }
                                        [ (con list) (con data) ]
                                      }
                                      tup_2258
                                    ]
                                  ]
                                  (con integer 0)
                                ]
                              ]
                              (lam ds_2263 (con unit) x_2262)
                            ]
                            (lam ds_2266 (con unit) x_2265)
                          ]
                          unitval_1820
                        ]
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_2250
            (fun (con data) (con data))
          )
          (lam d_2249 (con data) d_2249)
        )
        (termbind
          (nonstrict)
          (vardecl x_2154 Bool_1903)
          [
            { error_1825 Bool_1903 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataBool_cunsafeFromBuiltinData_2158
            (fun (con data) Bool_1903)
          )
          (lam
            d_2149
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl index_2150 (con integer))
                [
                  { { fst_1960 (con integer) } [ (con list) (con data) ] }
                  [ unsafeDataAsConstr_1949 d_2149 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2156 Bool_1903)
                [
                  [
                    [
                      [
                        { ifThenElse_1956 (fun (con unit) Bool_1903) }
                        [ [ equalsInteger_1963 index_2150 ] (con integer 0) ]
                      ]
                      (lam ds_2152 (con unit) False_1905)
                    ]
                    (lam ds_2155 (con unit) x_2154)
                  ]
                  unitval_1820
                ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) Bool_1903) }
                      [ [ equalsInteger_1963 index_2150 ] (con integer 1) ]
                    ]
                    (lam ds_2151 (con unit) True_1904)
                  ]
                  (lam ds_2157 (con unit) x_2156)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataExtended_cunsafeFromBuiltinData_2146
            (all
              a_2147
              (type)
              (fun
                [ (lam a_2148 (type) (fun (con data) a_2148)) a_2147 ]
                (fun (con data) [ Extended_1897 a_2147 ])
              )
            )
          )
          (abs
            a_2128
            (type)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl x_2140 [ Extended_1897 a_2128 ])
                [
                  { error_1825 [ Extended_1897 a_2128 ] }
                  [
                    {
                      [
                        Unit_match_1831
                        [
                          [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                          Unit_1830
                        ]
                      ]
                      (con unit)
                    }
                    unitval_1820
                  ]
                ]
              )
              (lam
                dUnsafeFromData_2129
                [ (lam a_2130 (type) (fun (con data) a_2130)) a_2128 ]
                (lam
                  d_2131
                  (con data)
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl
                        tup_2132
                        [
                          [ (con pair) (con integer) ] [ (con list) (con data) ]
                        ]
                      )
                      [ unsafeDataAsConstr_1949 d_2131 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl index_2133 (con integer))
                      [
                        { { fst_1960 (con integer) } [ (con list) (con data) ] }
                        tup_2132
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2142 [ Extended_1897 a_2128 ])
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_1956
                                (fun (con unit) [ Extended_1897 a_2128 ])
                              }
                              [
                                [ equalsInteger_1963 index_2133 ]
                                (con integer 0)
                              ]
                            ]
                            (lam ds_2138 (con unit) { NegInf_1899 a_2128 })
                          ]
                          (lam ds_2141 (con unit) x_2140)
                        ]
                        unitval_1820
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2135 a_2128)
                      [
                        dUnsafeFromData_2129
                        [
                          { head_1958 (con data) }
                          [
                            {
                              { snd_1952 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2132
                          ]
                        ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2136 [ Extended_1897 a_2128 ])
                      [ { Finite_1898 a_2128 } x_2135 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2144 [ Extended_1897 a_2128 ])
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_1956
                                (fun (con unit) [ Extended_1897 a_2128 ])
                              }
                              [
                                [ equalsInteger_1963 index_2133 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2137 (con unit) x_2136)
                          ]
                          (lam ds_2143 (con unit) x_2142)
                        ]
                        unitval_1820
                      ]
                    )
                    [
                      [
                        [
                          [
                            {
                              ifThenElse_1956
                              (fun (con unit) [ Extended_1897 a_2128 ])
                            }
                            [
                              [ equalsInteger_1963 index_2133 ] (con integer 2)
                            ]
                          ]
                          (lam ds_2134 (con unit) { PosInf_1900 a_2128 })
                        ]
                        (lam ds_2145 (con unit) x_2144)
                      ]
                      unitval_1820
                    ]
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataInterval_cunsafeFromBuiltinData_2188
            (all
              a_2189
              (type)
              (fun
                [ (lam a_2190 (type) (fun (con data) a_2190)) a_2189 ]
                (fun (con data) [ UpperBound_1907 a_2189 ])
              )
            )
          )
          (abs
            a_2175
            (type)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl x_2186 [ UpperBound_1907 a_2175 ])
                [
                  { error_1825 [ UpperBound_1907 a_2175 ] }
                  [
                    {
                      [
                        Unit_match_1831
                        [
                          [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                          Unit_1830
                        ]
                      ]
                      (con unit)
                    }
                    unitval_1820
                  ]
                ]
              )
              (lam
                dUnsafeFromData_2176
                [ (lam a_2177 (type) (fun (con data) a_2177)) a_2175 ]
                (lam
                  d_2178
                  (con data)
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl
                        tup_2179
                        [
                          [ (con pair) (con integer) ] [ (con list) (con data) ]
                        ]
                      )
                      [ unsafeDataAsConstr_1949 d_2178 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl t_2180 [ (con list) (con data) ])
                      [
                        { { snd_1952 (con integer) } [ (con list) (con data) ] }
                        tup_2179
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2182 Bool_1903)
                      [
                        fUnsafeFromDataBool_cunsafeFromBuiltinData_2158
                        [
                          { head_1958 (con data) }
                          [ { tail_1950 (con data) } t_2180 ]
                        ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2181 [ Extended_1897 a_2175 ])
                      [
                        [
                          {
                            fUnsafeFromDataExtended_cunsafeFromBuiltinData_2146
                            a_2175
                          }
                          dUnsafeFromData_2176
                        ]
                        [ { head_1958 (con data) } t_2180 ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2183 [ UpperBound_1907 a_2175 ])
                      [ [ { UpperBound_1908 a_2175 } x_2181 ] x_2182 ]
                    )
                    [
                      [
                        [
                          [
                            {
                              ifThenElse_1956
                              (fun (con unit) [ UpperBound_1907 a_2175 ])
                            }
                            [
                              [
                                equalsInteger_1963
                                [
                                  {
                                    { fst_1960 (con integer) }
                                    [ (con list) (con data) ]
                                  }
                                  tup_2179
                                ]
                              ]
                              (con integer 0)
                            ]
                          ]
                          (lam ds_2184 (con unit) x_2183)
                        ]
                        (lam ds_2187 (con unit) x_2186)
                      ]
                      unitval_1820
                    ]
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataInterval_cunsafeFromBuiltinData_2172
            (all
              a_2173
              (type)
              (fun
                [ (lam a_2174 (type) (fun (con data) a_2174)) a_2173 ]
                (fun (con data) [ LowerBound_1911 a_2173 ])
              )
            )
          )
          (abs
            a_2159
            (type)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl x_2170 [ LowerBound_1911 a_2159 ])
                [
                  { error_1825 [ LowerBound_1911 a_2159 ] }
                  [
                    {
                      [
                        Unit_match_1831
                        [
                          [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                          Unit_1830
                        ]
                      ]
                      (con unit)
                    }
                    unitval_1820
                  ]
                ]
              )
              (lam
                dUnsafeFromData_2160
                [ (lam a_2161 (type) (fun (con data) a_2161)) a_2159 ]
                (lam
                  d_2162
                  (con data)
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl
                        tup_2163
                        [
                          [ (con pair) (con integer) ] [ (con list) (con data) ]
                        ]
                      )
                      [ unsafeDataAsConstr_1949 d_2162 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl t_2164 [ (con list) (con data) ])
                      [
                        { { snd_1952 (con integer) } [ (con list) (con data) ] }
                        tup_2163
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2166 Bool_1903)
                      [
                        fUnsafeFromDataBool_cunsafeFromBuiltinData_2158
                        [
                          { head_1958 (con data) }
                          [ { tail_1950 (con data) } t_2164 ]
                        ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2165 [ Extended_1897 a_2159 ])
                      [
                        [
                          {
                            fUnsafeFromDataExtended_cunsafeFromBuiltinData_2146
                            a_2159
                          }
                          dUnsafeFromData_2160
                        ]
                        [ { head_1958 (con data) } t_2164 ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2167 [ LowerBound_1911 a_2159 ])
                      [ [ { LowerBound_1912 a_2159 } x_2165 ] x_2166 ]
                    )
                    [
                      [
                        [
                          [
                            {
                              ifThenElse_1956
                              (fun (con unit) [ LowerBound_1911 a_2159 ])
                            }
                            [
                              [
                                equalsInteger_1963
                                [
                                  {
                                    { fst_1960 (con integer) }
                                    [ (con list) (con data) ]
                                  }
                                  tup_2163
                                ]
                              ]
                              (con integer 0)
                            ]
                          ]
                          (lam ds_2168 (con unit) x_2167)
                        ]
                        (lam ds_2171 (con unit) x_2170)
                      ]
                      unitval_1820
                    ]
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataInterval_cunsafeFromBuiltinData_2204
            (all
              a_2205
              (type)
              (fun
                [ (lam a_2206 (type) (fun (con data) a_2206)) a_2205 ]
                (fun (con data) [ Interval_1915 a_2205 ])
              )
            )
          )
          (abs
            a_2191
            (type)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl x_2202 [ Interval_1915 a_2191 ])
                [
                  { error_1825 [ Interval_1915 a_2191 ] }
                  [
                    {
                      [
                        Unit_match_1831
                        [
                          [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                          Unit_1830
                        ]
                      ]
                      (con unit)
                    }
                    unitval_1820
                  ]
                ]
              )
              (lam
                dUnsafeFromData_2192
                [ (lam a_2193 (type) (fun (con data) a_2193)) a_2191 ]
                (lam
                  d_2194
                  (con data)
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl
                        tup_2195
                        [
                          [ (con pair) (con integer) ] [ (con list) (con data) ]
                        ]
                      )
                      [ unsafeDataAsConstr_1949 d_2194 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl t_2196 [ (con list) (con data) ])
                      [
                        { { snd_1952 (con integer) } [ (con list) (con data) ] }
                        tup_2195
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2198 [ UpperBound_1907 a_2191 ])
                      [
                        [
                          {
                            fUnsafeFromDataInterval_cunsafeFromBuiltinData_2188
                            a_2191
                          }
                          dUnsafeFromData_2192
                        ]
                        [
                          { head_1958 (con data) }
                          [ { tail_1950 (con data) } t_2196 ]
                        ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2197 [ LowerBound_1911 a_2191 ])
                      [
                        [
                          {
                            fUnsafeFromDataInterval_cunsafeFromBuiltinData_2172
                            a_2191
                          }
                          dUnsafeFromData_2192
                        ]
                        [ { head_1958 (con data) } t_2196 ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2199 [ Interval_1915 a_2191 ])
                      [ [ { Interval_1916 a_2191 } x_2197 ] x_2198 ]
                    )
                    [
                      [
                        [
                          [
                            {
                              ifThenElse_1956
                              (fun (con unit) [ Interval_1915 a_2191 ])
                            }
                            [
                              [
                                equalsInteger_1963
                                [
                                  {
                                    { fst_1960 (con integer) }
                                    [ (con list) (con data) ]
                                  }
                                  tup_2195
                                ]
                              ]
                              (con integer 0)
                            ]
                          ]
                          (lam ds_2200 (con unit) x_2199)
                        ]
                        (lam ds_2203 (con unit) x_2202)
                      ]
                      unitval_1820
                    ]
                  )
                )
              )
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl x_2125 TxInInfo_1893)
          [
            { error_1825 TxInInfo_1893 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2095 TxOut_1882)
          [
            { error_1825 TxOut_1882 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (nonstrict)
          (vardecl x_2076 Address_1879)
          [
            { error_1825 Address_1879 }
            [
              {
                [
                  Unit_match_1831
                  [
                    [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                    Unit_1830
                  ]
                ]
                (con unit)
              }
              unitval_1820
            ]
          ]
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2031
            (all
              a_2032
              (type)
              (fun
                [ (lam a_2033 (type) (fun (con data) a_2033)) a_2032 ]
                (fun (con data) [ Maybe_1863 a_2032 ])
              )
            )
          )
          (abs
            a_2016
            (type)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl x_2027 [ Maybe_1863 a_2016 ])
                [
                  { error_1825 [ Maybe_1863 a_2016 ] }
                  [
                    {
                      [
                        Unit_match_1831
                        [
                          [ { trace_1821 Unit_1829 } reconstructCaseError_1955 ]
                          Unit_1830
                        ]
                      ]
                      (con unit)
                    }
                    unitval_1820
                  ]
                ]
              )
              (lam
                dUnsafeFromData_2017
                [ (lam a_2018 (type) (fun (con data) a_2018)) a_2016 ]
                (lam
                  d_2019
                  (con data)
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl
                        tup_2020
                        [
                          [ (con pair) (con integer) ] [ (con list) (con data) ]
                        ]
                      )
                      [ unsafeDataAsConstr_1949 d_2019 ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl index_2021 (con integer))
                      [
                        { { fst_1960 (con integer) } [ (con list) (con data) ] }
                        tup_2020
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2029 [ Maybe_1863 a_2016 ])
                      [
                        [
                          [
                            [
                              {
                                ifThenElse_1956
                                (fun (con unit) [ Maybe_1863 a_2016 ])
                              }
                              [
                                [ equalsInteger_1963 index_2021 ]
                                (con integer 1)
                              ]
                            ]
                            (lam ds_2025 (con unit) { Nothing_1865 a_2016 })
                          ]
                          (lam ds_2028 (con unit) x_2027)
                        ]
                        unitval_1820
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2022 a_2016)
                      [
                        dUnsafeFromData_2017
                        [
                          { head_1958 (con data) }
                          [
                            {
                              { snd_1952 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2020
                          ]
                        ]
                      ]
                    )
                    (termbind
                      (nonstrict)
                      (vardecl x_2023 [ Maybe_1863 a_2016 ])
                      [ { Just_1864 a_2016 } x_2022 ]
                    )
                    [
                      [
                        [
                          [
                            {
                              ifThenElse_1956
                              (fun (con unit) [ Maybe_1863 a_2016 ])
                            }
                            [
                              [ equalsInteger_1963 index_2021 ] (con integer 0)
                            ]
                          ]
                          (lam ds_2024 (con unit) x_2023)
                        ]
                        (lam ds_2030 (con unit) x_2029)
                      ]
                      unitval_1820
                    ]
                  )
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataAddress_cunsafeFromBuiltinData_2078
            (fun (con data) Address_1879)
          )
          (lam
            d_2068
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2069
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2068 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2070 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2069
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2072 [ Maybe_1863 StakingCredential_1875 ])
                [
                  [
                    {
                      fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2031
                      StakingCredential_1875
                    }
                    fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                  ]
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2070 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2071 Credential_1871)
                [
                  fUnsafeFromDataCredential_cunsafeFromBuiltinData_2048
                  [ { head_1958 (con data) } t_2070 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2073 Address_1879)
                [ [ Address_1880 x_2071 ] x_2072 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) Address_1879) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2069
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2074 (con unit) x_2073)
                  ]
                  (lam ds_2077 (con unit) x_2076)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataMap_1987
            (all
              v_1988
              (type)
              (all
                k_1989
                (type)
                (fun Unit_1829 [ List_1841 [ [ Tuple2_1848 k_1989 ] v_1988 ] ])
              )
            )
          )
          (abs
            v_1984
            (type)
            (abs
              k_1985
              (type)
              (lam
                ds_1986 Unit_1829 { Nil_1842 [ [ Tuple2_1848 k_1985 ] v_1984 ] }
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            unsafeDataAsMap_1983
            (fun
              (con data) [ (con list) [ [ (con pair) (con data) ] (con data) ] ]
            )
          )
          (builtin unMapData)
        )
        (termbind
          (strict)
          (vardecl
            chooseList_1969
            (all
              a_1970
              (type)
              (all
                b_1971
                (type)
                (fun [ (con list) a_1970 ] (fun b_1971 (fun b_1971 b_1971)))
              )
            )
          )
          (builtin chooseList)
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataMap_cunsafeFromBuiltinData_2006
            (all
              k_2007
              (type)
              (all
                v_2008
                (type)
                (fun
                  [ (lam a_2009 (type) (fun (con data) a_2009)) k_2007 ]
                  (fun
                    [ (lam a_2010 (type) (fun (con data) a_2010)) v_2008 ]
                    (fun
                      (con data)
                      [
                        [
                          (lam
                            k_2011
                            (type)
                            (lam
                              v_2012
                              (type)
                              [ List_1841 [ [ Tuple2_1848 k_2011 ] v_2012 ] ]
                            )
                          )
                          k_2007
                        ]
                        v_2008
                      ]
                    )
                  )
                )
              )
            )
          )
          (abs
            k_1990
            (type)
            (abs
              v_1991
              (type)
              (lam
                dUnsafeFromData_1992
                [ (lam a_1993 (type) (fun (con data) a_1993)) k_1990 ]
                (lam
                  dUnsafeFromData_1994
                  [ (lam a_1995 (type) (fun (con data) a_1995)) v_1991 ]
                  (let
                    (rec)
                    (termbind
                      (strict)
                      (vardecl
                        go_1997
                        (fun
                          [
                            (con list) [ [ (con pair) (con data) ] (con data) ]
                          ]
                          [ List_1841 [ [ Tuple2_1848 k_1990 ] v_1991 ] ]
                        )
                      )
                      (lam
                        l_1998
                        [ (con list) [ [ (con pair) (con data) ] (con data) ] ]
                        (let
                          (nonrec)
                          (termbind
                            (nonstrict)
                            (vardecl
                              lvl_2003
                              [ List_1841 [ [ Tuple2_1848 k_1990 ] v_1991 ] ]
                            )
                            [
                              go_1997
                              [
                                {
                                  tail_1950
                                  [ [ (con pair) (con data) ] (con data) ]
                                }
                                l_1998
                              ]
                            ]
                          )
                          (termbind
                            (nonstrict)
                            (vardecl
                              tup_1999 [ [ (con pair) (con data) ] (con data) ]
                            )
                            [
                              {
                                head_1958
                                [ [ (con pair) (con data) ] (con data) ]
                              }
                              l_1998
                            ]
                          )
                          (termbind
                            (nonstrict)
                            (vardecl lvl_2001 v_1991)
                            [
                              dUnsafeFromData_1994
                              [
                                { { snd_1952 (con data) } (con data) } tup_1999
                              ]
                            ]
                          )
                          (termbind
                            (nonstrict)
                            (vardecl lvl_2000 k_1990)
                            [
                              dUnsafeFromData_1992
                              [
                                { { fst_1960 (con data) } (con data) } tup_1999
                              ]
                            ]
                          )
                          (termbind
                            (nonstrict)
                            (vardecl lvl_2002 [ [ Tuple2_1848 k_1990 ] v_1991 ])
                            [
                              [ { { Tuple2_1849 k_1990 } v_1991 } lvl_2000 ]
                              lvl_2001
                            ]
                          )
                          (termbind
                            (nonstrict)
                            (vardecl
                              lvl_2004
                              [ List_1841 [ [ Tuple2_1848 k_1990 ] v_1991 ] ]
                            )
                            [
                              [
                                { Cons_1843 [ [ Tuple2_1848 k_1990 ] v_1991 ] }
                                lvl_2002
                              ]
                              lvl_2003
                            ]
                          )
                          [
                            [
                              [
                                [
                                  {
                                    {
                                      chooseList_1969
                                      [ [ (con pair) (con data) ] (con data) ]
                                    }
                                    (fun
                                      Unit_1829
                                      [
                                        List_1841
                                        [ [ Tuple2_1848 k_1990 ] v_1991 ]
                                      ]
                                    )
                                  }
                                  l_1998
                                ]
                                { { fUnsafeFromDataMap_1987 v_1991 } k_1990 }
                              ]
                              (lam ds_2005 Unit_1829 lvl_2004)
                            ]
                            Unit_1830
                          ]
                        )
                      )
                    )
                    (lam
                      d_1996
                      (con data)
                      [ go_1997 [ unsafeDataAsMap_1983 d_1996 ] ]
                    )
                  )
                )
              )
            )
          )
        )
        (termbind
          (nonstrict)
          (vardecl
            fUnsafeFromDataValue_2013
            (fun
              (con data)
              [
                [
                  (lam
                    k_2014
                    (type)
                    (lam
                      v_2015
                      (type)
                      [ List_1841 [ [ Tuple2_1848 k_2014 ] v_2015 ] ]
                    )
                  )
                  (con bytestring)
                ]
                (con integer)
              ]
            )
          )
          [
            [
              {
                {
                  fUnsafeFromDataMap_cunsafeFromBuiltinData_2006
                  (con bytestring)
                }
                (con integer)
              }
              unsafeDataAsB_1967
            ]
            unsafeDataAsI_1966
          ]
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2097
            (fun (con data) TxOut_1882)
          )
          (lam
            d_2079
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2080
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2079 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2081 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2080
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2082 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2081 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2091 [ Maybe_1863 (con bytestring) ])
                [
                  [
                    {
                      fUnsafeFromDataMaybe_cunsafeFromBuiltinData_2031
                      (con bytestring)
                    }
                    unsafeDataAsB_1967
                  ]
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2082 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl
                  x_2086
                  [
                    [
                      (lam
                        k_2087
                        (type)
                        (lam
                          v_2088
                          (type)
                          [ List_1841 [ [ Tuple2_1848 k_2087 ] v_2088 ] ]
                        )
                      )
                      (con bytestring)
                    ]
                    [
                      [
                        (lam
                          k_2089
                          (type)
                          (lam
                            v_2090
                            (type)
                            [ List_1841 [ [ Tuple2_1848 k_2089 ] v_2090 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      (con integer)
                    ]
                  ]
                )
                [
                  [
                    [
                      {
                        {
                          fUnsafeFromDataMap_cunsafeFromBuiltinData_2006
                          (con bytestring)
                        }
                        [
                          [
                            (lam
                              k_2084
                              (type)
                              (lam
                                v_2085
                                (type)
                                [ List_1841 [ [ Tuple2_1848 k_2084 ] v_2085 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      }
                      unsafeDataAsB_1967
                    ]
                    fUnsafeFromDataValue_2013
                  ]
                  [ { head_1958 (con data) } t_2082 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2083 Address_1879)
                [
                  fUnsafeFromDataAddress_cunsafeFromBuiltinData_2078
                  [ { head_1958 (con data) } t_2081 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2092 TxOut_1882)
                [ [ [ TxOut_1883 x_2083 ] x_2086 ] x_2091 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) TxOut_1882) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2080
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2093 (con unit) x_2092)
                  ]
                  (lam ds_2096 (con unit) x_2095)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2127
            (fun (con data) TxInInfo_1893)
          )
          (lam
            d_2117
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2118
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2117 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2119 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2118
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2121 TxOut_1882)
                [
                  fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2097
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2119 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2120 TxOutRef_1890)
                [
                  fUnsafeFromDataTxOutRef_cunsafeFromBuiltinData_2116
                  [ { head_1958 (con data) } t_2119 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2122 TxInInfo_1893)
                [ [ TxInInfo_1894 x_2120 ] x_2121 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) TxInInfo_1893) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2118
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2123 (con unit) x_2122)
                  ]
                  (lam ds_2126 (con unit) x_2125)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            unsafeDataAsList_1968 (fun (con data) [ (con list) (con data) ])
          )
          (builtin unListData)
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataNil_cunsafeFromBuiltinData_1980
            (all
              a_1981
              (type)
              (fun
                [ (lam a_1982 (type) (fun (con data) a_1982)) a_1981 ]
                (fun (con data) [ List_1841 a_1981 ])
              )
            )
          )
          (abs
            a_1972
            (type)
            (lam
              dUnsafeFromData_1973
              [ (lam a_1974 (type) (fun (con data) a_1974)) a_1972 ]
              (let
                (rec)
                (termbind
                  (strict)
                  (vardecl
                    go_1976 (fun [ (con list) (con data) ] [ List_1841 a_1972 ])
                  )
                  (lam
                    l_1977
                    [ (con list) (con data) ]
                    [
                      [
                        [
                          [
                            {
                              { chooseList_1969 (con data) }
                              (fun Unit_1829 [ List_1841 a_1972 ])
                            }
                            l_1977
                          ]
                          (lam ds_1978 Unit_1829 { Nil_1842 a_1972 })
                        ]
                        (lam
                          ds_1979
                          Unit_1829
                          [
                            [
                              { Cons_1843 a_1972 }
                              [
                                dUnsafeFromData_1973
                                [ { head_1958 (con data) } l_1977 ]
                              ]
                            ]
                            [ go_1976 [ { tail_1950 (con data) } l_1977 ] ]
                          ]
                        )
                      ]
                      Unit_1830
                    ]
                  )
                )
                (lam
                  d_1975 (con data) [ go_1976 [ unsafeDataAsList_1968 d_1975 ] ]
                )
              )
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2310
            (fun (con data) TxInfo_1929)
          )
          (lam
            d_2272
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2273
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2272 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2274 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2273
                ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2275 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2274 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2276 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2275 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2277 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2276 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2278 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2277 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2279 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2278 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2280 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2279 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2281 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2280 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2282 [ (con list) (con data) ])
                [ { tail_1950 (con data) } t_2281 ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2304 (con bytestring))
                [
                  fUnsafeFromDataTxId_cunsafeFromBuiltinData_2105
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2282 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl
                  x_2303
                  [ List_1841 [ [ Tuple2_1848 (con bytestring) ] (con data) ] ]
                )
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980
                      [ [ Tuple2_1848 (con bytestring) ] (con data) ]
                    }
                    [
                      [
                        {
                          {
                            fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2267
                            (con bytestring)
                          }
                          (con data)
                        }
                        unsafeDataAsB_1967
                      ]
                      fUnsafeFromDataBuiltinData_cunsafeFromBuiltinData_2250
                    ]
                  ]
                  [ { head_1958 (con data) } t_2282 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2302 [ List_1841 (con bytestring) ])
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980
                      (con bytestring)
                    }
                    unsafeDataAsB_1967
                  ]
                  [ { head_1958 (con data) } t_2281 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2301 [ Interval_1915 (con integer) ])
                [
                  [
                    {
                      fUnsafeFromDataInterval_cunsafeFromBuiltinData_2204
                      (con integer)
                    }
                    unsafeDataAsI_1966
                  ]
                  [ { head_1958 (con data) } t_2280 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl
                  x_2300
                  [
                    List_1841
                    [ [ Tuple2_1848 StakingCredential_1875 ] (con integer) ]
                  ]
                )
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980
                      [ [ Tuple2_1848 StakingCredential_1875 ] (con integer) ]
                    }
                    [
                      [
                        {
                          {
                            fUnsafeFromDataTuple2_cunsafeFromBuiltinData_2267
                            StakingCredential_1875
                          }
                          (con integer)
                        }
                        fUnsafeFromDataStakingCredential_cunsafeFromBuiltinData_2067
                      ]
                      unsafeDataAsI_1966
                    ]
                  ]
                  [ { head_1958 (con data) } t_2279 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2299 [ List_1841 DCert_1920 ])
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980 DCert_1920
                    }
                    fUnsafeFromDataDCert_cunsafeFromBuiltinData_2248
                  ]
                  [ { head_1958 (con data) } t_2278 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl
                  x_2294
                  [
                    [
                      (lam
                        k_2295
                        (type)
                        (lam
                          v_2296
                          (type)
                          [ List_1841 [ [ Tuple2_1848 k_2295 ] v_2296 ] ]
                        )
                      )
                      (con bytestring)
                    ]
                    [
                      [
                        (lam
                          k_2297
                          (type)
                          (lam
                            v_2298
                            (type)
                            [ List_1841 [ [ Tuple2_1848 k_2297 ] v_2298 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      (con integer)
                    ]
                  ]
                )
                [
                  [
                    [
                      {
                        {
                          fUnsafeFromDataMap_cunsafeFromBuiltinData_2006
                          (con bytestring)
                        }
                        [
                          [
                            (lam
                              k_2292
                              (type)
                              (lam
                                v_2293
                                (type)
                                [ List_1841 [ [ Tuple2_1848 k_2292 ] v_2293 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      }
                      unsafeDataAsB_1967
                    ]
                    fUnsafeFromDataValue_2013
                  ]
                  [ { head_1958 (con data) } t_2277 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl
                  x_2287
                  [
                    [
                      (lam
                        k_2288
                        (type)
                        (lam
                          v_2289
                          (type)
                          [ List_1841 [ [ Tuple2_1848 k_2288 ] v_2289 ] ]
                        )
                      )
                      (con bytestring)
                    ]
                    [
                      [
                        (lam
                          k_2290
                          (type)
                          (lam
                            v_2291
                            (type)
                            [ List_1841 [ [ Tuple2_1848 k_2290 ] v_2291 ] ]
                          )
                        )
                        (con bytestring)
                      ]
                      (con integer)
                    ]
                  ]
                )
                [
                  [
                    [
                      {
                        {
                          fUnsafeFromDataMap_cunsafeFromBuiltinData_2006
                          (con bytestring)
                        }
                        [
                          [
                            (lam
                              k_2285
                              (type)
                              (lam
                                v_2286
                                (type)
                                [ List_1841 [ [ Tuple2_1848 k_2285 ] v_2286 ] ]
                              )
                            )
                            (con bytestring)
                          ]
                          (con integer)
                        ]
                      }
                      unsafeDataAsB_1967
                    ]
                    fUnsafeFromDataValue_2013
                  ]
                  [ { head_1958 (con data) } t_2276 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2284 [ List_1841 TxOut_1882 ])
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980 TxOut_1882
                    }
                    fUnsafeFromDataTxOut_cunsafeFromBuiltinData_2097
                  ]
                  [ { head_1958 (con data) } t_2275 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2283 [ List_1841 TxInInfo_1893 ])
                [
                  [
                    {
                      fUnsafeFromDataNil_cunsafeFromBuiltinData_1980
                      TxInInfo_1893
                    }
                    fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2127
                  ]
                  [ { head_1958 (con data) } t_2274 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2305 TxInfo_1929)
                [
                  [
                    [
                      [
                        [
                          [
                            [
                              [ [ [ TxInfo_1930 x_2283 ] x_2284 ] x_2287 ]
                              x_2294
                            ]
                            x_2299
                          ]
                          x_2300
                        ]
                        x_2301
                      ]
                      x_2302
                    ]
                    x_2303
                  ]
                  x_2304
                ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) TxInfo_1929) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2273
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2306 (con unit) x_2305)
                  ]
                  (lam ds_2309 (con unit) x_2308)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2346
            (fun (con data) ScriptContext_1946)
          )
          (lam
            d_2336
            (con data)
            (let
              (nonrec)
              (termbind
                (nonstrict)
                (vardecl
                  tup_2337
                  [ [ (con pair) (con integer) ] [ (con list) (con data) ] ]
                )
                [ unsafeDataAsConstr_1949 d_2336 ]
              )
              (termbind
                (nonstrict)
                (vardecl t_2338 [ (con list) (con data) ])
                [
                  { { snd_1952 (con integer) } [ (con list) (con data) ] }
                  tup_2337
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2340 ScriptPurpose_1940)
                [
                  fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2335
                  [
                    { head_1958 (con data) } [ { tail_1950 (con data) } t_2338 ]
                  ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2339 TxInfo_1929)
                [
                  fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2310
                  [ { head_1958 (con data) } t_2338 ]
                ]
              )
              (termbind
                (nonstrict)
                (vardecl x_2341 ScriptContext_1946)
                [ [ ScriptContext_1947 x_2339 ] x_2340 ]
              )
              [
                [
                  [
                    [
                      { ifThenElse_1956 (fun (con unit) ScriptContext_1946) }
                      [
                        [
                          equalsInteger_1963
                          [
                            {
                              { fst_1960 (con integer) }
                              [ (con list) (con data) ]
                            }
                            tup_2337
                          ]
                        ]
                        (con integer 0)
                      ]
                    ]
                    (lam ds_2342 (con unit) x_2341)
                  ]
                  (lam ds_2345 (con unit) x_2344)
                ]
                unitval_1820
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl checkHasFailedError_1837 (con string))
          (con string "PT5")
        )
        (termbind
          (strict)
          (vardecl
            traceError_1835 (all a_1836 (type) (fun (con string) a_1836))
          )
          (abs
            a_1832
            (type)
            (lam
              str_1833
              (con string)
              [
                { error_1825 a_1832 }
                [
                  {
                    [
                      Unit_match_1831
                      [ [ { trace_1821 Unit_1829 } str_1833 ] Unit_1830 ]
                    ]
                    (con unit)
                  }
                  unitval_1820
                ]
              ]
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            wrapMintingPolicy_2358
            (all
              r_2359
              (type)
              (fun
                [ (lam a_2360 (type) (fun (con data) a_2360)) r_2359 ]
                (fun
                  (fun r_2359 (fun ScriptContext_1946 Bool_1903))
                  (fun (con data) (fun (con data) Unit_1829))
                )
              )
            )
          )
          (abs
            r_2347
            (type)
            (lam
              dUnsafeFromData_2348
              [ (lam a_2349 (type) (fun (con data) a_2349)) r_2347 ]
              (lam
                f_2350
                (fun r_2347 (fun ScriptContext_1946 Bool_1903))
                (lam
                  r_2351
                  (con data)
                  (lam
                    p_2352
                    (con data)
                    {
                      [
                        [
                          {
                            [
                              Bool_match_1906
                              [
                                [ f_2350 [ dUnsafeFromData_2348 r_2351 ] ]
                                [
                                  fUnsafeFromDataScriptContext_cunsafeFromBuiltinData_2346
                                  p_2352
                                ]
                              ]
                            ]
                            (all dead_2354 (type) Unit_1829)
                          }
                          (abs dead_2355 (type) Unit_1830)
                        ]
                        (abs
                          dead_2356
                          (type)
                          [
                            { traceError_1835 Unit_1829 }
                            checkHasFailedError_1837
                          ]
                        )
                      ]
                      (all dead_2357 (type) dead_2357)
                    }
                  )
                )
              )
            )
          )
        )
        [
          [ { wrapMintingPolicy_2358 Unit_1829 } fUnsafeFromDataUnit_2418 ]
          mkPolicyTrue_2376
        ]
      )
    )
  )
)