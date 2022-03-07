# UPLC Polymorphic builtins

```k
require "uplc-configuration.md"

module UPLC-POLYMORPHIC-BUILTINS
  imports UPLC-CONFIGURATION
```

## `ifThenElse`

```k
  rule <k> (builtin ifThenElse) ~> Force => #ITE ... </k>

  rule <k> (V:Value ~> ([ Clos(#ITE, _RHO) _])) => #ITE(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#ITE(V1:Value), _RHO) _])) => #ITE(V1, V2) ... </k>

  rule <k> (V3:Value ~> ([ Clos(#ITE(V1:Value, V2:Value), _RHO) _])) => #ITE(V1, V2, V3) ... </k>

  rule <k> #ITE((con bool True), V1:Value, _V2:Value) => V1 ... </k>

  rule <k> #ITE((con bool False), _V1:Value, V2:Value) => V2 ... </k>
```

## `chooseUnit`

```k
  rule <k> (builtin chooseUnit) ~> Force => #CUT ... </k>

  rule <k> (V:Value ~> ([ Clos(#CUT, _RHO) _])) => #CUT(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#CUT(V1:Value), _RHO) _])) => #CUT(V1, V2) ... </k>

  rule <k> #CUT((con unit ()), V:Value) => V ... </k>
```

## `trace`

```k
  rule <k> (builtin trace) ~> Force => #TRC ... </k>

  rule <k> (V:Value ~> ([ Clos(#TRC, _RHO) _])) => #TRC(V) ... </k>

  rule <k> (V2:Value ~> ([ Clos(#TRC(V1:Value), _RHO) _])) => #TRC(V1, V2) ... </k>

  rule <k> #TRC((con string S), V:Value) => V </k>
       <trace> ... (.List => ListItem(S)) </trace>
```

```k
endmodule
``` 
