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

- `[(con integer) s]` is a type but `s` is a size (which is not a type.)
- Underspecified grammar, for example [ Term TermList ] doesn't parse
- `addInteger` test has no size
- `(fix ...)` rule changes type (Fig. 9)
- In syntax, (Fig. 2), lambda terms take in Type Values, but in the semantics (Fig. 9), they take in any Type

