---
title: KPlutus kick-off meeting
author: | 
   Christiano Braga and Erik Kaneda  
   Semantics Lead \& Compiler Engineer  
   \{christiano.braga, erik.kaneda\}@runtimeverification.com  

date: Dec. 1st., 2021
header-includes:
	- '\usepackage{fourier}'
	- '\usetheme[numbering=fraction]{metropolis}'
	- '\usepackage{graphicx}'
	- '\titlegraphic{\includegraphics[width=4cm]{rv-logo}}'
---

# Objective

  - To present our initial understanding about the project and Plutus' semantics.

# Plutus semantics project

  - What is the project: a K semantics for Plutus Core
  - Why the project: enable RV to perform audits on Plutus smartcontracts
  - How will we develop it: Semantics, testing and real-world
  - When will develop it: From Jan 2022 to Oct 2022

<!-- # Plutus example -->

<!-- - The example below calculates the absolute value of a number. -->
<!-- - Plutus has an eager semantics. To delay delay the -->
<!--   evaluation of the branches, they can be made into functions taking -->
<!--   arguments of type unit that are applied only when  -->
<!--   the outcome of the test is known: -->

<!--   $\includegraphics[width=\textwidth]{plutus-example.pdf}$ -->

# Untyped Plutus Core

  - Suitable for onchain verification
  - Obtained after type-erasure of typed Plutus
  - Main components
    * Reduction semantics
	* Terms
	* Frames
	* Configuration: a stack of Frames, an environment of Variables to
      Closures, and Terms
	* Reduction relations: $\rhd$ and $\lhd$

# Dynamic semantics

$\includegraphics[width=\textwidth]{fig-23.pdf}$

# Questions {.allowframebreaks}

  - What is the semantics for closure unfolding?
  - What do overlined variables mean in builtin evaluation? 
  - Is there an existing repository of untyped plutus programs that we
    could use as tests to aid in our development? 
  - The paragraph of Appendix C states that “A later version of this
    document will contain a specification for evaluation of built-in
    functions which will allow us to evaluate arbitrary untyped plutus
    core terms”. When will this part of the specification be released? 
    * Section C.1.1 has some requirements for built-in function. Is
      this the specification? 
    * Section 5 discusses built-in types/functions/terms but likens
      some operations to operators in haskell. 
    * order of operations for arithmetic is implied by the nesting of
      the terms. 
  - “A future version of this document will include a cost model which
    will provide fine-grained costs for individual operations and
    built-in functions, enabling accurate (dynamic) monitoring of
    execution costs.” 
 
