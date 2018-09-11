In this repository we implement the Plutus Core specification defined [here][spec].
At the momemnt we target version `v1.0rc4.4`.

Installation
============

After installing [pandoc] and [ninja-build] as dependencies, simply run `./build`
to setup OCaml, K, compile the semantics and run the tests. 

The file `plutus-core.md` defines the Plutus Core language, and the file
`plutus-core-spec.md` defines unit tests as a reachability specification.

[spec]:        https://github.com/psygnisfive/Plutus-Core-Spec
[pandoc]:      https://pandoc.org
[ninja-build]: https://ninja-build.org

Errors identified in specification / reference implementation
=============================================================

Reference implementation
------------------------

- Integers builtins overflow too quickly in the specification. [integer-overflow](Github issue)

[integer-overflow]: https://github.com/input-output-hk/plutus-prototype/issues/96

Specification:
--------------

- In syntax, (Fig. 2), lambdas and other constructs take in Type Values, but in the semantics
  (Fig. 9), they take in any Type. [lambda-should-take-types-not-tyvalues](Commit that fixes this.)

- Underspecified grammar, for example [ Term TermList ] doesn't parse

- `(fix ...)` rule changes type (Fig. 9)

- Is there a reason `(error ...)` is not a value? It seems to make more sense within the context of the rest of the semantics.

- resizeInteger and intToByteString: arguments say i has size s1, should it be s0?

- `intToByteString`: Behaviour for negative integers is not specified. Which binary representation
  should we use?

[lambda-should-take-types-not-tyvalues]: https://github.com/psygnisfive/Plutus-Core-Spec/commit/1dcc2fdf330b685e39157ec8a159701b68952227

