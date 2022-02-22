# UPLC Polymorphic builtins

```k
require "uplc-configuration.md"

module UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-CONFIGURATION
```

## `ifThenElse`

```k
  rule <k> (builtin ifThenElse .TermList) => V1 </k>
       <args> ... (ListItem((con bool True))
                   ListItem(V1:Value)
                   ListItem(_V2:Value)) => .List </args>

  rule <k> (builtin ifThenElse .TermList) => V2 </k>
       <args> ... ((ListItem((con bool False))
                    ListItem(_V1:Value)
                    ListItem(V2:Value)) => .List) </args>

  rule <k> (builtin ifThenElse) => #ITE ... </k>

  rule <k> (V:Value ~> ([ Clos(#ITE, _RHO) _])) => #ITE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ITE(V1:Value), _RHO) _])) => #ITE(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#ITE(V1:Value, V2:Value), _RHO) _])) => #ITE(V1, V2, V3) ... </k>

  rule <k> #ITE((con bool True), V1:Value, _V2:Value) => V1 </k>

  rule <k> #ITE((con bool False), _V1:Value, V2:Value) => V2 </k>
```

## `trace`

```k
  rule <k> (builtin trace .TermList) => V1 </k>
       <args> ... (ListItem((con bool True))
                   ListItem(V1:Value)
                   ListItem(_V2:Value)) => .List </args>

  rule <k> (builtin ifThenElse .TermList) => V2 </k>
       <args> ... ((ListItem((con bool False))
                    ListItem(_V1:Value)
                    ListItem(V2:Value)) => .List) </args>

  rule <k> (builtin ifThenElse) => #ITE ... </k>

  rule <k> (V:Value ~> ([ Clos(#ITE, _RHO) _])) => #ITE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ITE(V1:Value), _RHO) _])) => #ITE(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#ITE(V1:Value, V2:Value), _RHO) _])) => #ITE(V1, V2, V3) ... </k>

  rule <k> #ITE((con bool True), V1:Value, _V2:Value) => V1 </k>

  rule <k> #ITE((con bool False), _V1:Value, V2:Value) => V2 </k>
```


```k
endmodule
``` 
