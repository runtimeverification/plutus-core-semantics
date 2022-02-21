# String builtins 

```k
require "uplc-configuration.md"
require "uplc-bytestring.md"
require "uplc-string.md"

module UPLC-STRING-BUILTINS
  imports UPLC-CONFIGURATION
  imports UPLC-BYTESTRING
  imports UPLC-STRING
  imports K-EQUAL
```

## `encodeUtf8`

```k 
  rule <k> (builtin encodeUtf8 .TermList) =>
           (con bytestring #encodeUtf8(S)) </k>  
       <stack> ... (ListItem((con string S:String)) => .List) </stack>

  rule <k> (builtin encodeUtf8) => #EUTF ... </k>

  rule <k> (V:Value ~> ([ Clos(#EUTF, _RHO) _])) => #EUTF(V) ... </k>

  rule <k> #EUTF((con string S:String)) =>
           (con bytestring #encodeUtf8(S)) </k>
```

## `decodeUtf8`

```k 
  rule <k> (builtin decodeUtf8 .TermList) =>
           (con string #decodeUtf8(B)) </k>  
       <stack> ... (ListItem((con bytestring B:ByteString)) => .List) </stack>

  rule <k> (builtin decodeUtf8) => #DUTF ... </k>

  rule <k> (V:Value ~> ([ Clos(#DUTF, _RHO) _])) => #DUTF(V) ... </k>

  rule <k> #DUTF((con bytestring B:ByteString)) =>
           (con string #decodeUtf8(B)) </k>
```

## `appendString`

```k 
  rule <k> (builtin appendString .TermList) =>
           (con string #appendString(S1, S2)) </k>  
       <stack> ... (ListItem((con string S1:String))
                    ListItem((con string S2:String)) => .List) </stack>

  rule <k> (builtin appendString) => #ASTR ... </k>

  rule <k> (V:Value ~> ([ Clos(#ASTR, _RHO) _])) => #ASTR(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ASTR(V1:Value), _RHO) _])) => #ASTR(V1, V2) ... </k>

  rule <k> #ASTR((con string S1:String), (con string S2:String)) =>
           (con string #appendString(S1, S2)) </k>
```

## `equalsString`

```k 
  rule <k> (builtin equalsString .TermList) =>
           (con bool
            #if #equalsString(S1, S2)
            #then (True)
            #else (False)
            #fi)
       </k>  
       <stack> ... (ListItem((con string S1:String))
                    ListItem((con string S2:String)) => .List) </stack>

  rule <k> (builtin equalsString) => #ESTR ... </k>

  rule <k> (V:Value ~> ([ Clos(#ESTR, _RHO) _])) => #ESTR(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ESTR(V1:Value), _RHO) _])) => #ESTR(V1, V2) ... </k>

  rule <k> #ESTR((con string S1:String), (con string S2:String)) =>
           (con bool
            #if #equalsString(S1, S2)
            #then (True)
            #else (False)
            #fi)
       </k>  
```

```k
endmodule
``` 