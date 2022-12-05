# List-sum: correctness claims

```k
requires "verification.md"

module CLAIMS
  imports VERIFICATION
```

## Issue 3377

```k
  claim
  <k>
    GO_NO_SCRIPT_IN_INPUT_CORE => gLookup(k_False) ...
  </k>
  <env>
    k_aux_go  |-> < lam k_x [ [ k_f [ k_s k_s ] ] k_x ]
                    k_f |-> < lam k_aux_go ( lam k_ds_438 GO_NO_SCRIPT_IN_INPUT_CORE )
                              k_scrHash |-> < con bytestring SH:ByteString >
                            >
                    k_s |-> < lam k_s ( lam k_x [ [ k_f [ k_s k_s ] ] k_x ] )
                              k_f |-> < lam k_aux_go ( lam k_ds_438 GO_NO_SCRIPT_IN_INPUT_CORE )
                                        k_scrHash |-> < con bytestring SH:ByteString >
                                      >
                            >
                  >
    k_ds_438  |-> DList("CustomTxInInfo", CL:ConstantList)
    k_scrHash |-> < con bytestring SH:ByteString >
      =>
    .Map
  </env>
  requires inList(SH, CustomTxInInfoListHashes(CL))
   andBool DestructConstantList(CL)
```

```k
endmodule
```