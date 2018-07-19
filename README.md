In this repository we implement the Plutus Core specification defined [here][spec].
At the momemnt we target version `v1.0rc4.4`.

# Installation

After installing [pandoc] and [ninja-build] as dependencies, simply run `./build`
to setup OCaml, K, compile the semantics and run the tests. 

The file `plutus-core.md` defines the Plutus Core language, and the file
`plutus-core-spec.md` defines unit tests as a reachability specification.

[spec]:        https://github.com/psygnisfive/Plutus-Core-Spec
[pandoc]:      https://pandoc.org
[ninja-build]: https://ninja-build.org
