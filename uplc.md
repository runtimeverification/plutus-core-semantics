K Semantics of UPLC
===================

This is a draft K implementation of the CEK machine for Untyped (or typed erased)
Plutus Core (UPLC), following [version
2.1](https://hydra.iohk.io/build/8205579/download/1/plutus-core-specification.pdf)
of its specification, which will be henceforth refered as IOG's Plutus
Core specification or simply IOG's specification.

# Lexical grammar

Module `PLUTUS-CORE-LEXICAL-GRAMMAR` defines the lexical grammar of
Plutus Core. (Note that there exists lexemes for types.)  It is almost
a literal translation of the lexical grammar in IOG's specification.

## Representing Integers

We use K's builtin type `Int` to represent Plutus' `Integer` lexeme.

Lines
```
syntax Integer      ::= r"[+-]?[0-9]+"                [token] // int
syntax Constant     ::= ...
                      | Integer                               // integer constant
```
are replaced by
```
imports UNSIGNED-INT-SYNTAX

syntax Constant     ::= ...
                      | Int
```

## Regexp for version

Also, the `Version` lexeme has been changed with respect to IOG's
specification as the `uplc` compiler only accepts tokens of the general form below.
```
[0-9]+.[0-9]+.[0-9]+
```
Examples are:
```
$ echo "(program 1.2.3 (con integer 5))" | ./uplc evaluate
(con integer 5)
$ echo "(program 1.2 (con integer 5))" | ./uplc evaluate
uplc: Unexpected '(' at line 1, column 14
$ echo "(program 1 (con integer 5))" | ./uplc evaluate
uplc: Unexpected '(' at line 1, column 12
$ echo "(program 1.2.3.4 (con integer 5))" | ./uplc evaluate
uplc: Unexpected '.' at line 1, column 15
```

We replace
```
syntax Version      ::= r"[0-9]+(.[0-9]+)*"           [token] // version
```
for the regexp below.
```
syntax Version      ::= r"[0-9]+.[0-9]+.[0-9]+"       [token] // updated version lexeme  
```

## The Lexical grammar

```k
requires "domains.md"

module UNTYPED-PLUTUS-CORE-LEXICAL-GRAMMAR
   imports UNSIGNED-INT-SYNTAX
   
   syntax Name         ::= r"[a-zA-Z][a-zA-Z0-9\\_\\']*" [token] // name
   syntax Var          ::= Name                                  // term variable
   syntax TyVar        ::= Name                                  // type variable
   syntax BuiltinName  ::= Name                                  // builtin term name
   syntax ByteString   ::= r"#([a-fA-F0-9][a-fA-F0-9])+" [token] // hex string
   syntax Version      ::= r"[0-9]+.[0-9]+.[0-9]+"       [token] // updated version lexeme  
   syntax Constant     ::= "()"                                  // unit constant
                         | "True" | "False"                      // boolean constant
                         | Int                                   // K builtin integer 
                         | ByteString                            // bytestring constant
   syntax TypeConstant ::= Name                                  // type constant
endmodule
```

# Syntax grammar

Module `UNTYPED-PLUTUS-CORE-GRAMMAR` is responsible for the
specification of the syntax of UPLC. An UPLC program is essentially a
pair formed by a version string together with a term. A term can be
simply a variable, a value, an application, a `force` expression on a
delayed term, a call to a builtin operation or an error. Last but not
least, values can be constants, lambda abstractions or the delaying of
a given term.

```k
module UNTYPED-PLUTUS-CORE-GRAMMAR
   imports UNTYPED-PLUTUS-CORE-LEXICAL-GRAMMAR
   imports LIST
   imports BOOL

   syntax Value ::= "(" "con" TypeConstant Constant ")" // constant
                  | "(" "lam" Var Term ")"              // lambda abstraction
                  | "(" "delay" Term ")"                // delay execution of a term

   syntax TermList ::= NeList{Term, ""}

   syntax Term ::= Var
                 | Value 
                 | "[" Term Term "]"                     // function application
                 | "(" "force" Term ")"                  // force execution of a term
                 | "(" "builtin" BuiltinName TermList ")"// builtin
                 | "(" "error" ")"                       // error

   syntax Program ::= "(" "program" Version Term ")"     // versioned program

endmodule
```

# Abstract syntax

```k
module UNTYPED-PLUTUS-CORE-ABSTRACT-SYNTAX
  imports LIST
  imports MAP
  imports UNTYPED-PLUTUS-CORE-LEXICAL-GRAMMAR
  
  syntax AValue ::= Con(TypeConstant, Constant)
                  | Lam(Var, ATerm)
		  | Delay(ATerm)
		  
  syntax ATerm ::= AValue
		 | App(ATerm, ATerm)
		 | Force(ATerm)
		 | Builtin(BuiltinName, List)
		 | "Error" 
		 | Final(AValue, Map)
endmodule

module UNTYPED-PLUTUS-CORE-PARSER
  imports UNTYPED-PLUTUS-CORE-GRAMMAR
  imports UNTYPED-PLUTUS-CORE-ABSTRACT-SYNTAX

  syntax ATerm ::= parse(Program)
                 | parse(Term) 
		 | parse(Value)
		 | parse(Term, List)

  rule <k> parse((program _V T:Term)) => parse(T) ... </k>
  rule <k> parse((con Ty C)) => Con(Ty, C) ... </k>
  rule <k> parse((builtin BN T:Term TL:TermList)) => parse(T) </k>
       <stack> 

  // rule <k> parse((builtin BN (T:Term TL:TermList))) =>
  //          parse((builtin BN TL), ListItem(T)) ... </k>

  // rule <k> parse((builtin BN (T:Term TL:TermList)), L) =>
  //          parse((builtin BN TL), (L ListItem(T))) ... </k>

  // rule <k> parse((builtin BN .TermList), L) => Builtin(BN, L) ... </k>
  
  configuration <k> parse($PGM:Term) </k>
                <stack> .List </stack>
endmodule

module UPLC
  imports UNTYPED-PLUTUS-CORE-PARSER
endmodule
```

// # Dynamic semantics of UPLC

// A CEK machine is a an abstract stack machine used in IOG's
// specification for the dynamic semantics of Untyped Plutus
// Core. Essentially, each reduction rule in the semantics relates
// _states_ comprised by the stack frame, the environment and a third
// component that can be either a term or a value. 

// ```k
// module UNTYPED-PLUTUS-CORE-CEK
//   imports UNTYPED-PLUTUS-CORE-GRAMMAR
//   imports MAP
//   imports LIST
//   imports INT
// ```

// ## ATerms

// We associate a term rewriting system with the CEK machine of IOG's
// specification where the terms being rewritten are elements of set
// `ATerm` together with the environment and the stack, explained below.

// ```k
// ```

// ## AClosure

// A value is pushed onto the stack carrying the current environment, thus
// forming a closure, that will be taking into account while retrieving it
// from the stack.

// ```k
//   syntax AClosure ::= Clos(Value, Map)
// ```

// ## AFrame

// Frames are used to record an evaluation context. For instance, an
// application term `[M N]` is carried on by first evaluating `M` and
// then `N` is evaluated. Therefore, before the evaluation of `M` starts,
// the frame `[_ N]`, called right application, is pushed onto the stack
// to record the fact that `N` must be evaluated next. The left
// application frame `[ AClosure _]` has a similar meaning. The evaluation
// of `M`, in an application `[M N]`, should yield a closure that will
// then be applied to result of the evaluation of `N`. Frames
// `BuiltinApp` and `Force` have similar meaning: to keep a record of the
// context an evaluation is taking place.

// ```k
//   syntax AFrame   ::= "[_" Term "]"
//                     | "[" AClosure "_]"
//                     | BuiltinApp(BuiltinName, List, TermList, Map)
//                     | "Force"
// ```

// ## States

// States are represented in K Plutus using K cells. Terms and values go into
// `<k>`, the envionment into `<env>` and `the stack into `stack`.

// ```k
//   configuration <k> $PGM:ATerm </k>
//                 <env> .Map </env>
// 		<stack> .List </stack>
// ```

// ### Final states

// IOG's specification defines `<>` as an error state and `[](V, \rho)`
// as non-error final state. Here the error state is denoted by configuration
// ```
// <k> <> </k> <env> .Map </env> <stack> .List </stack>
// ```
// and a non-error final state by configuration
// ```
// <k> [] V RHO </k> <env> .Map </env> <stack> .List </stack>
// ```

// ## Evaluation rules

// A reduction rule in IOG's specification follows one of the following forms.
// 1. `s; \rho |> M |-> s'; \rho' |> N`
// 1. `s; \rho |> M |-> s'; \rho' <| V`
// 1. `s; \rho |> M |-> <>`
// 1. `s; \rho <| V |-> s'; \rho' |> M`
// 1. `. ; \rho <| V |-> [](V, \rho)`

// In K they are represented as rules in the following general form,
// ```
// rule <k> M => N <k>
//      <env> RHO => RHO' </env>
//      <stack> S => S' <stack>
// ```
// where the K cell has (a stack of) terms, the env cell is a map
// of variables and values and the stack cell is a list of frames.

// The rules that coherce a value from state `s, \rho |> V` to `s, \rho
// <| V` are actually not needed as states are all coaptured by K's-three
// -cells-configuration (k, env, stack) described above. Therefore, the
// following rules are implicit in this version of the K specification of UPLC.
// ```
// s ; \rho |> (con tn cn) |-> s ; \rho <| (con tn cn)
// s ; \rho |> (lam x M)   |-> s ; \rho <| (lam x M)
// s ; \rho |> (delay M)   |-> s ; \rho <| (delay M)
// ```

// ### Program & final states

// The evaluation of a program simply forgets about the version and
// evaluates the enclosed term.
// ```k
//   rule <k> (program _V M) => M ... </k>
// ```

// The error rule in IOG's specification
// ```
// s ; \rho |> error |-> <>
// ```
// is encoded in K as follows.
// ```k
//   rule <k> (error) => <> ... </k>
//        <env> _RHO  => .Map   </env>
//        <stack> _S  => .List  </stack>
// ```

// And so is the rule for non-error final state.
// ```
// . ; \rho <| V |-> [](V, \rho)
// ```
// ```
//   rule <k> V:Value   => [] V RHO </k>
//        <env> RHO     => .Map     </env>
//        <stack> .List             </stack>
// ```
// However, `uplc` compiler does not produce the syntax `[] V
// \rho`. Therefore, in the current specification we consider
// ```
// <k> V </k> <env> RHO </env> <stack> .List </stack>
// ```
// to be the non-error final states.

// ### Main rules

// The following rules should be self-explanatory given what has been
// said so far. It is perhaps worth mentioning at this point that we use
// K´s semantic lists (which are polymorphic) to represent the frame
// stack. Therefore, we need to use the `ListItem` constructor to
// represent frame stack elements. The pattern
// ```
// ... (.List => ListItem(F)) 
// ```
// denotes pushing the frame `F` to the top (right end of the list) to
// the frame stack.
// ```k
//   rule <k> X:Var => V ... </k>
//        <env> (_RHO X |-> Clos(V, RHO')) => RHO' </env>

//   rule <k> (force M:Term) => M ... </k>
//        <stack> ... (.List => ListItem(Force)) </stack>

//   rule <k> [ M N ] => M ... </k>
//        <stack> ... (.List => ListItem([_ N])) </stack>

//   rule <k> V:Value => N ... </k>
//        <env> RHO </env>
//        <stack> ... (ListItem([_ N:Term]) =>
//                     ListItem([ Clos(V, RHO) _])) </stack>

//   rule <k> V:Value => M ... </k>
//        <env> RHO => RHO' (X |-> Clos(V, RHO)) </env>
//        <stack> ... (ListItem([ Clos((lam X:Var M), RHO') _]) => .List) </stack>
       
//   rule <k> (delay M:Term) => M </k>
//        <stack> ... (ListItem(Force) => .List) </stack>
// ```

// ### Builtins

// TODO: Builtins are not being handled properly yet.

// ```k
//   syntax TypeConstant ::= "integer" 

//   syntax BuiltinName ::= "addInteger" 
  
//   // s ; \rho |> (builtin bn) |-> s ; \rho |> M (bn computes to M)

//   rule <k> (builtin BN:BuiltinName ( M:Term Ms:TermList ) ) => M ... </k>
//        <env> RHO </env>
//        <stack> ... (.List => ListItem(BuiltinApp(BN, .List, Ms, RHO))) </stack>

//   // s , ((builtin bn Cs _ M Ms), \rho') ; \rho <| V    |->
//   // s , ((builtin bn Cs(V, \rho) _ Ms), \rho') ; \rho' |> M
//   rule <k> V:Value => M </k>
//        <env> RHO => RHO' </env>
//        <stack> ... (ListItem(BuiltinApp(BN, C, (M Ms), RHO')) =>
//                     ListItem(BuiltinApp(BN, (C ListItem(Clos(V, RHO))), Ms, RHO')))
//        </stack>

//   // s , ((builtin bn Cs _), \rho') ; \rho <| V  |->
//   // s ; \rho' |> M  (bn computes on Cs (V, \rho) to M)

//   rule <k> (con T:TypeConstant I2) => (con T (I1 +Int I2)) </k>
//        <env> _ => RHO' </env>
//        <stack> ... (ListItem(BuiltinApp(addInteger, ListItem(Clos((con T I1:Int), _RHO)), .TermList, RHO')) => .List) </stack>
// endmodule

// module UPLC
//     imports UNTYPED-PLUTUS-CORE-GRAMMAR
//     imports UNTYPED-PLUTUS-CORE-CEK
// endmodule
// ```
