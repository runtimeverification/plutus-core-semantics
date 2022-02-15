# Bytestring builtins

```k
require "uplc-configuration.md"
require "bytestring.md"

module BYTESTRING-BUILTINS
  imports UPLC-CONFIGURATION
  imports BYTESTRING
  imports K-EQUAL
```

## `appendByteString`


```k
  rule <k> (builtin appendByteString .TermList) =>
           (con bytestring #appendByteString(B1, B2)) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin appendByteString) => #ABS ... </k>

  rule <k> (V:Value ~> ([ Clos(#ABS, _RHO) _])) => #ABS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ABS(V1:Value), _RHO) _])) => #ABS(V1, V2) ... </k>

  rule <k> #ABS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bytestring #appendByteString(B1, B2)) ... </k>
```

## `consByteString`

```k
  rule <k> (builtin consByteString .TermList) =>
           (con bytestring #consByteString(I, B)) ... </k>
       <stack> ... (ListItem((con integer I:Int))
                    ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin consByteString) => #CBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#CBS, _RHO) _])) => #CBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CBS(V1:Value), _RHO) _])) => #CBS(V1, V2) ... </k>

  rule <k> #CBS((con integer I:Int), (con bytestring B:ByteString)) =>
           (con bytestring #consByteString(I, B)) ... </k>
```

## `sliceByteString`

```k
  rule <k> (builtin sliceByteString .TermList) =>
           (con bytestring #sliceByteString(I1, I2, B)) ... </k>
       <stack> ... (ListItem((con integer I1:Int))
                    ListItem((con integer I2:Int))
                    ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin sliceByteString) => #SBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#SBS, _RHO) _])) => #SBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#SBS(V1:Value), _RHO) _])) => #SBS(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#SBS(V1:Value, V2:Value), _RHO) _])) => #SBS(V1, V2, V3) ... </k>

  rule <k> #SBS((con integer I1:Int), (con integer I2:Int), (con bytestring B:ByteString)) =>
           (con bytestring #sliceByteString(I1, I2, B)) ... </k>
```

## `lengthOfByteString`

```k
  rule <k> (builtin lengthOfByteString .TermList) =>
           (con bytestring #lengthOfByteString(B)) ... </k>
       <stack> ... (ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin lengthOfByteString) => #LBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LBS, _RHO) _])) => #LBS(V) ... </k>

  rule <k> #LBS((con bytestring B:ByteString)) =>
           (con integer #lengthOfByteString(B)) ... </k>
```

## `indexByteString`

```k
  rule <k> (builtin indexByteString .TermList) =>
           (con bytestring #indexByteString(B, I)) ... </k>
       <stack> ... (ListItem((con bytestring B:ByteString))
                    ListItem((con integer I:Int)) => .List) </stack>

  rule <k> (builtin indexByteString) => #IBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#IBS, _RHO) _])) => #IBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#IBS(V1:Value), _RHO) _])) => #IBS(V1, V2) ... </k>

  rule <k> #IBS((con bytestring B:ByteString), (con integer I:Int)) =>
           (con integer #indexByteString(B, I)) ... </k>
```

## `equalsByteString`

```k
  rule <k> (builtin equalsByteString .TermList) =>
           (con bool
	    #if (#equalsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin equalsByteString) => #EBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#EBS, _RHO) _])) => #EBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#EBS(V1:Value), _RHO) _])) => #EBS(V1, V2) ... </k>

  rule <k> #EBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#equalsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
```

## `lessThanByteString`

```k
  rule <k> (builtin lessThanByteString .TermList) =>
           (con bool
	    #if (#lessThanByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin lessThanByteString) => #LTBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LTBS, _RHO) _])) => #LTBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LTBS(V1:Value), _RHO) _])) => #LTBS(V1, V2) ... </k>

  rule <k> #LTBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#lessThanByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
```

## `lessThanEqualsByteString`

```k
  rule <k> (builtin lessThanEqualsByteString .TermList) =>
           (con bool
	    #if (#lessThanEqualsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
       <stack> ... (ListItem((con bytestring B1:ByteString))
                    ListItem((con bytestring B2:ByteString)) => .List) </stack>

  rule <k> (builtin lessThanEqualsByteString) => #LEBS ... </k>

  rule <k> (V:Value ~> ([ Clos(#LEBS, _RHO) _])) => #LEBS(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#LEBS(V1:Value), _RHO) _])) => #LEBS(V1, V2) ... </k>

  rule <k> #LEBS((con bytestring B1:ByteString), (con bytestring B2:ByteString)) =>
           (con bool
	    #if (#lessThanEqualsByteString(B1, B2) ==Bool true)
	    #then (True)
	    #else (False)
	    #fi) ... </k>
```

```k
endmodule
```