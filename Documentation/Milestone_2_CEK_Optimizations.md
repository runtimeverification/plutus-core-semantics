---
documentclass: article
title: KPlutus Milestone 2 Optimizations
author: |
  Petar Maksimović \
  Runtime Verification Inc. \
  KPlutus Team
date: Sep. 7th, 2022
header-includes:
  - \usepackage[T1]{fontenc}
  - \usepackage{fourier}
  - \usepackage{fullpage}
---

# Milestone 2 CEK Optimizations

## Environment cutting

This is a high-level justification that the environment cutting preserves the CEK semantics.

We define the environment cutting function as follows:

```
# : Env x Term -> Env

#(rho, M) = { x |-> rho(x) | x in dom(rho), x in FV(M) }
```

This function cuts a given environment to only the free variables of a given term. The intuition is that the bindings that remain are the only ones relevant for evaluating the term.

The first property we prove is:

**Claim 1**. For all terms `M` and environments `rho`, it holds that ```M @ rho = M @ #(rho, M)```.

This claim means that the result of the iterated substitution `@` only depends on the free variables of `M`. The proof follows directly from the definition of `@` and the properties of substitution.

Next, we extend the # function to values:
```
             #(< con tn C >) = < con tn C >
          #(< delay M rho >) = < delay M #(rho, M) >
          #(< lam x M rho >) = < lam x M #(rho, (lam x M)) >
#(< builtin b V1 ... Vn e >) = < builtin b #(V1) ... #(Vn) e >
```
and prove that the discharge function is unaffected by the cutting:

**Claim 2**. For all values `V`, it holds that ```U(V) = U(#(V))```.

The proof is done by structural induction on `V` and uses Claim 1.

Next, we define alternative rules for the CEK semantics that involve environment cutting, denoting the new transition relation by |->#. The following three rules replace their counterparts from the spec:
```
s; rho |> (delay M) |-># < delay M #(rho, M) >
s; rho |> (lam x M) |-># < lam x M #(rho, (lam x M)) >
s; rho |> [ M N ]   |-># [_ (N, #(rho, N)) ] . s; #(rho, M) |> M
```
whereas all of the other rules remain the same.

Next, we extend the # function to states:
```
#(s; rho |> M) = #(s); #(rho, M) |> M
     #(s <| V) = #(s) <| #(V)
         #(<>) = <>
        #([]V) = [](#(V))
```
where the extension of # to stacks is as follows:
```
  #([ ]) = [ ]
#(f . s) = #(f) . #(s)
```
and to frames is as follows:
```
  #((force _)) = (force _)
#([_ (M, rho)) = [_ (M, #(rho, M)]
      #([V _]) = [#(V) _]
```

We then prove that the two semantics (one with cutting (|->#) and one without (|->) move in lock-step:

**Claim 3a**. For all states `S` and `S'`, it holds that:
```
       S |-> S' => #(S) |-># #(S')
#(S) |-># #(S') => S |-> S'
```

The proof is by case analysis on `S |-> S'` for the first part, and by case analysis on `#(S) |-># #(S')` for the second part.

Next, we extend Claim 3b inductively to obtain

**Claim 3b** For all states `S` and `S'`, it holds that:
```
       S |->n S' => #(S) |->#n #(S')
#(S) |->#n #(S') => S |->n S'
```
where `|->n` and `|->#n` denote `n`-step executions in the two CEK semantics, defined in the standard way. The proof is by induction on `n`, and uses Claim 3a.

As a corollary of Claim 3b, noting that `#([ ]; [ ] |> M) = [ ]; [ ] |> M`, we have:

**Corollary 1**. For all terms `M` and values `V`, it holds that:
```
[ ]; [ ] |> M |->> []V <=> [ ]; [ ] |> M |->># [](#(V))
[ ]; [ ] |> M |->> <>  <=> [ ]; [ ] |> M |->># <>
```
where `|->>` and `|->>#` denote to-termination executions of the two semantics.

This corollary, together with Claim 2, means that if one of the semantics terminates with a given final result when executing a given program, the other will also terminate with the same result. By contraposition, if the execution of a given program does not terminate in one of the semantics, it will also not terminate in the other one.

KPlutus currently implements the `|->#` semantics, meaning that the environment is always cut.

## Global environment

When analyzing compiled code, the size of the environment that is passed around affects the performance of KPlutus. To tackle this issue, we observe the structure of the compiled code, which is roughly of the form:
```
[ (lam x1
  ...
    [ (lam xn M) Mn ]
  ...
  M1 ]
```
where `M` is the uplc code corresponding to the compiled code, and `x1, ..., xn` are identifiers associated with the encoding of the associated datatypes and functions. Given this format, `M` will be evaluated in an environment `rho = { xi |-> vi | i = 1, ..., n }`, where `vi` are the appropriate evaluations of `Mi`.
As the properties of the Scott encoding ensure that these identifiers are unique (meaning that they are not re-bound in `M`), we have that the bindings for `xi` will remain in `rho` during the evaluation of `M`. We make use of this property to factor out (thus far manually, in future automatically) the bindings for `xi` into a new (static) state component that we call the *global environment* and denote by `G`. We adapt the CEK machine identifier lookup rules as follows:
```
s, rho |> x |->G G(x),   when x is bound in G
s, rho |> x |->G rho(x), when x is not bound in G and is bound in rho
s, rho |> x |->G <>,     otherwise
```
where `|->G` denotes the adapted CEK semantics with respect to a global environment `G`.

Unlike the environment-cutting optimization, the global environment optimization is not always sound. However, as per the argument above, it is sound for our use case, given the structure of the compiled code and the uniqueness of `x1, ..., xn`.