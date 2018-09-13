In this repository we implement the Plutus Core specification defined [here][spec].
The IOHK Plutus implementation is [here][prototype].
At the moment we target version `v1.0 (RC5.3)`.

Installation
============

After installing [pandoc] and [ninja-build] as dependencies, simply run `./build`
to setup OCaml, K, compile the semantics and run the tests. 

The file `plutus-core.md` defines the Plutus Core language, and the file
`plutus-core-spec.md` defines unit tests as a reachability specification.

[spec]:        https://github.com/psygnisfive/Plutus-Core-Spec
[prototype]:   https://github.com/input-output-hk/plutus-prototype
[pandoc]:      https://pandoc.org
[ninja-build]: https://ninja-build.org

Errors / Questions from specification / reference implementation
================================================================

Reference implementation
------------------------

- Integers builtins overflow too quickly in the specification.

  > Fixed: [Commit that fixes this.][integer-overflow]

[integer-overflow]: https://github.com/plutus-prototype/commit/407dd1b964b40fe11fc90fa8354420020246b58a

Specification
-------------

- `(fix ...)` as a term construct is still present. See pgs 12, 13, 15.

- What does `V` refer to in the second to last case in Figure 12?

- In syntax, (Fig. 2), lambdas and other constructs take in Type Values, but in the semantics
  (Fig. 9), they take in any Type
  > Fixed: [Commit that fixes this.][lambda-should-take-types-not-tyvalues]

- Underspecified grammar, for example [ Term TermList ] doesn't parse.
  > Grammar follows what internal AST representation is

- `(fix ...)` rule changes type (Fig. 9)
  > Deprecated, `(fix ...)` is no longer a term-level construct.

- resizeInteger and intToByteString: arguments say i has size s1, should it be s0?
  > Fixed: [Commit that fixes this.][correct-arguments]

- `intToByteString`: Behaviour for negative integers is not specified. Which binary representation
  should we use?
  > Two’s complement. Not yet added to spec.

[lambda-should-take-types-not-tyvalues]: https://github.com/psygnisfive/Plutus-Core-Spec/commit/1dcc2fdf330b685e39157ec8a159701b68952227
[correct-arguments]: https://github.com/psygnisfive/Plutus-Core-Spec/commit/2ff1f0a65c72b93561d291d1faca23280cbd09e1

