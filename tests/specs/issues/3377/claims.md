```k
requires "verification.md"

module CLAIMS
  imports VERIFICATION
```

# Issue 3377

To reproduce the issue, you need to install `KPlutus`, using the `haskell-backend-issues` branch, ideally just by following the instructions in its `README` file.  When successful, run `make tests/specs/issues/3377/claims.md.prove`, wait for successful compilation, but stop the process once `kprove` is triggered. Then, run `kplc prove --directory tests/specs/issues/3377/verification/haskell tests/specs/issues/3377/claims.md --debugger --debug-script k-script` to get into the repl. You will be proving the following claim

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

but it will have already been split into two claims by the Haskell backend.

The `GO_NO_SCRIPT_IN_INPUT_CORE` UPLC function effectively traverses the list `CL`, which containing records of PlutusTx type `CustomTxInInfo`, looks into each of those records extracting a specific hash, and returns false if there is a record in the list whose hash equals `SH`.

The key part of the claim is the requires clause with `inList(SH, CustomTxInInfoListHashes(CL))`, where `inList(X, L)` is a predicate that returns `true` iff the element `X` is in the list `L`, and `CustomTxInInfoListHashes(CL)` is a function that takes a list of `CustomTxInInfo` records and returns the list of their hashes. Both `inList` and `CustomTxInInfoListHashes` are defined in [this file](./verification.md).

The problem with proving this claim is in the fact that it requires type inversion (that is, case-splitting on whether `CL` is an empty list or not) that the code triggers in a way that's incompatible with the representation of `CL`. In particular, the list `CL` is encoded in UPLC as `DList("CustomTxInInfo", CL:ConstantList)` [defined here](./uplc-genvironment-instance.md). The `DList` abstraction originates from the PlutusTx list type, with the usual constructors `Nil` and `Cons`, but in UPLC it is just a complex lambda-term and any inversion is, so far, handled manually, by writing manual `#Ceil` simplifications (illustrated [here](./uplc-genvironment-instance.md) for the `Credential` datatype). The Haskell backend uses these simplifications to case-split at the top level, at this point creating multiple claims. However, for recursive datatypes such as lists, such a simplification would result in an infinite unrolling loop and cannot be done for `DList`. For this reason, we resort to a trick and perform the case-splitting manually, using the `DestructConstantList(CL)` function, defined in [this file](./verification.md).

## Running the claim

When in the repl, you will start from:

```
Kore (0)>
```

Run `step 500`, and after some seconds, you 'll get to

```
Stopped after 228 step(s) due to branching on [229,230,231]
```

and 229 and 230 will discharge immediately, leaving the (unprovable) branch at 231. When you look at the konfig at 228, you will see

```
  #Ceil ( DList ( "CustomTxInInfo" , DLCT:ConstantList ) )
#And
  <generatedTop>
    <k>
      GO_NO_SCRIPT_IN_INPUT_CORE ~> _DotVar1:K
    </k>
    <env>
      k_aux_go |-> < lam k_x [ [ k_f [ k_s k_s ] ] k_x ] k_f |-> < lam k_aux_go ( lam k_ds_438 GO_NO_SCRIPT_IN_INPUT_CORE ) k_scrHash |-> < con bytestring SH:ByteString > >
      k_s |-> < lam k_s ( lam k_x [ [ k_f [ k_s k_s ] ] k_x ] ) k_f |-> < lam k_aux_go ( lam k_ds_438 GO_NO_SCRIPT_IN_INPUT_CORE ) k_scrHash |-> < con bytestring SH:ByteString > > > >
      k_ds_438 |-> DList ( "CustomTxInInfo" , DLCT:ConstantList )
      k_scrHash |-> < con bytestring SH:ByteString >
    </env>
    <trace>
      _Gen3:List
    </trace>
  </generatedTop>
#And { true #Equals MaybeDef ( "StakingCredential" , ACSC:Int , APSC:ConstantList ) }
#And { true #Equals TxOutRefDef ( PARAMS_OR:ConstantList ) }
#And { true #Equals inList ( SH:ByteString , CustomTxInInfoListHashes ( DLCT:ConstantList ) ) }
```

which matches the original claim with `DCLT` (tail of `CL`) instead of `CL` and circularity should work. However, since the backend is attempting to apply the circularity using simplified claim, we get to the branch at 231, which has the following constraints:

```
  #Not ( {
      < delay ( lam k_case_Nil ( lam k_case_Cons [ [ k_case_Cons k_arg_0 ] k_arg_1 ] ) ) k_arg_0 |-> < delay ( lam k_case_CustomTxInInfo [ [ k_case_CustomTxInInfo k_arg_0 ] k_arg_1 ] ) k_arg_0 |-> < con data { TxOutRefData ( PARAMS_OR:ConstantList ) } >
      k_arg_1 |-> < delay ( lam k_case_CustomTxOut [ [ [ k_case_CustomTxOut k_arg_0 ] k_arg_1 ] k_arg_2 ] ) k_arg_0 |-> < con data { Constr 0 [ Constr 1 [ ByteString CBS:ByteString , .DataList ] , MaybeData ( "StakingCredential" , ACSC:Int , APSC:ConstantList ) , .DataList ] } >
      k_arg_1 |-> < con data { OV:TextualData } >
      k_arg_2 |-> < con data { OMDH:TextualData } > > >
      k_arg_1 |-> DList ( "CustomTxInInfo" , DLCT:ConstantList ) >
    #Equals
      DList ( "CustomTxInInfo" , DLCT:ConstantList )
    }
  #And
    {
      true
    #Equals
      inList ( SH:ByteString , CBS:ByteString , CustomTxInInfoListHashes ( DLCT:ConstantList ) )
    } )
#And
  #Not ( {
    < delay ( lam k_case_Nil ( lam k_case_Cons [ [ k_case_Cons k_arg_0 ] k_arg_1 ] ) ) k_arg_0 |-> < delay ( lam k_case_CustomTxInInfo [ [ k_case_CustomTxInInfo k_arg_0 ] k_arg_1 ] ) k_arg_0 |-> < con data { TxOutRefData ( PARAMS_OR:ConstantList ) } >
    k_arg_1 |-> < delay ( lam k_case_CustomTxOut [ [ [ k_case_CustomTxOut k_arg_0 ] k_arg_1 ] k_arg_2 ] ) k_arg_0 |-> < con data { Constr 0 [ Constr 0 [ ByteString CBS:ByteString , .DataList ] , MaybeData ( "StakingCredential" , ACSC:Int , APSC:ConstantList ) , .DataList ] } >
    k_arg_1 |-> < con data { OV:TextualData } >
    k_arg_2 |-> < con data { OMDH:TextualData } > > >
    k_arg_1 |-> DList ( "CustomTxInInfo" , DLCT:ConstantList ) >
  #Equals
    DList ( "CustomTxInInfo" , DLCT:ConstantList )
  } )
```

where both of the complex equalities are false, meaning that their negations are true, and the execution should continue (when it shoud not).

```k
endmodule
```