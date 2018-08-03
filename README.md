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

Percieved mistakes / shortcommings in the specification
=======================================================

- Are sizes supposed to be both Constants and TyConstants, or should they only be TyConstants

- Underspecified grammar, for example [ Term TermList ] doesn't parse
- `addInteger` test has no size
- `(fix ...)` rule changes type (Fig. 9)
- In syntax, (Fig. 2), lambda terms take in Type Values, but in the semantics (Fig. 9), they take in any Type

- Is there a reason `(error ...)` is not a value? It seems to make more sense within the context of the rest of the semantics.

- resizeInteger and intToByteString: arguments say i has size s1, should it be s0?

- `intToByteString`: Behaviour for negative integers is not specified. Which binary representation
  should we use?
