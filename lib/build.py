#!/usr/bin/env python3

from kninja import *

proj = KProject()

plutus_core = proj.source('plutus-core.md') \
                  .then(proj.tangle().output(proj.builddir('plutus-core.k')))
krypto = proj.source('ext/blockchain-k-plugin/plugin/krypto.md') \
             .then(proj.tangle().output(proj.builddir('krypto.k')))
def build_def(name, main_file, backend, main_module = None, syntax_module = None, flags = ""):
    if main_module   is not None: flags += ' --main-module '   + main_module
    if syntax_module is not None: flags += ' --syntax-module ' + syntax_module
    return main_file \
           .then(proj.kompile(backend = backend)
                     .variables(     flags = flags
                               , directory = proj.builddir(name)
                               )
                     .implicit([krypto])
                ) \
           .alias(name)

def build_ocaml_with_interpreter( name, main_file
                                , main_module = None, syntax_module = None
                                , kompile_flags = ''
                                , packages = []
                                ):
    directory = proj.builddir(name)
    if main_module   is not None: kompile_flags += ' --main-module '   + main_module
    if syntax_module is not None: kompile_flags += ' --syntax-module ' + syntax_module
    return proj.kompile_interpreter( main_file, directory
                                   , additional_ml_sources = [proj.source('ext/blockchain-k-plugin/plugin/HASH.ml')]
                                   , kompile_flags = kompile_flags
                                   , packages = packages
                                   ).alias(name)

lazy   = build_def( 'lazy'
                  , plutus_core
                  , backend = 'java'
                  , main_module = 'PLUTUS-CORE-LAZY'
                  , syntax_module = 'PLUTUS-CORE-SYNTAX'
                  )
strict = build_def( 'strict'
                  , plutus_core
                  , backend = 'java'
                  , main_module = 'PLUTUS-CORE-STRICT'
                  , syntax_module = 'PLUTUS-CORE-SYNTAX'
                  )

llvm_strict  = build_def( 'llvm-strict'
                                            , plutus_core
                  , backend = 'llvm'
                                            , main_module = 'PLUTUS-CORE-STRICT'
                                            , syntax_module = 'PLUTUS-CORE-SYNTAX'
                  , flags = '' # "-ccopt ../../../ext/blockchain-k-plugin/plugin-c/crypto.cpp -ccopt -lcryptopp"
                                            )
llvm_lazy = build_def( 'llvm-lazy'
                                         , plutus_core
                  , backend = 'llvm'
                                         , main_module = 'PLUTUS-CORE-LAZY'
                                         , syntax_module = 'PLUTUS-CORE-SYNTAX'
                  , flags = "-ccopt ../../../ext/blockchain-k-plugin/plugin-c/crypto.cpp -ccopt -lcryptopp"
                                         )

typing_k = proj.source('typing.md') \
               .then(proj.tangle().output(proj.builddir('typing.k')))
typing = build_def( 'typing'
                  , typing_k
                  , backend = 'java'
                  , main_module = 'PLUTUS-CORE-TYPING'
                  )

def do_test(defn, input, expected):
    return proj.source(input) \
               .then(defn.krun()) \
               .then(proj.check(proj.source(expected))) \
               .default()

lazy_tests = []
strict_tests = []
llvm_strict_tests = []
llvm_lazy_tests = []
typing_tests = []

def test(input):
    global lazy_tests, strict_tests, llvm_strict_tests, llvm_lazy_tests
    expected = input + '.expected'
    lazy_tests += [ do_test(lazy, input, expected) ]
    strict_tests += [ do_test(strict, input, expected) ]
    llvm_strict_tests += [ do_test(llvm_strict, input, expected) ]
    llvm_lazy_tests += [ do_test(llvm_lazy, input, expected) ]

def test_llvm(input):
    global llvm_strict_tests, llvm_lazy_tests
    expected = input + '.llvm.expected'
    llvm_strict_tests += [ do_test(llvm_strict, input, expected) ]
    llvm_lazy_tests   += [ do_test(llvm_lazy,   input, expected) ]

def test_java(input):
    global lazy_tests, strict_tests
    expected = input + '.java.expected'
    lazy_tests += [ do_test(lazy, input, expected) ]
    strict_tests += [ do_test(strict, input, expected) ]

def test_strict(input):
    global strict_tests, llvm_strict_tests
    expected = input + '.expected'
    strict_tests += [ do_test(strict, input, expected) ]
    llvm_strict_tests += [ do_test(llvm_strict, input, expected) ]

def test_typing(input):
    global typing_tests
    expected = input + '.expected'
    typing_tests += [ do_test(typing, input, expected) ]

# Basic tests
# -----------
#
# Since the OCaml does not support reachability claims (even
# concrete ones) these also function as smoke tests for the OCaml backend)
#
test('t/builtin-app.plc')
test('t/lambda-app.plc')

# We need distinct exptected files for these since bytestrings are
# pretty printed differently.
test_llvm('t/bytestring.plc')
test_java('t/bytestring.plc')

# Cryptography
# ------------
#
# We do not yet support hashing on the Java backend since the SHA3 hook does
# not exist, and the SHA2 hook is missing an alias into the HASH namespace.
#
test_llvm('t/sha2.plc')
# test_java('t/sha2.plc')
test_llvm('t/sha3.plc')
# test_java('t/sha3.plc')

# Complex tests
# -------------
#
# These are tests involving recursion, and other tests from the Roman and the
# IOHK Plutus team.
#
test_strict('t/11-scott-to-int.plc')
test('t/sum-list.plc')
test('t/sum.plc')
test('t/fact-simple.plc')
test('t/succ-integer.plc')
test('t/factorial.plc')
test('t/fibonacci.plc')

# Typing tests
# ------------

test_typing('t/typing/builtin-app.plc')

test_typing('t/typing/add-integer.plc')
test_typing('t/typing/error.plc')
test_typing('t/typing/int-id-app.plc')
test_typing('t/typing/int-id-poly-inst.plc')
test_typing('t/typing/int-id-poly.plc')
test_typing('t/typing/int-id.plc')
test_typing('t/typing/int.plc')

test_typing('t/typing/less-than.plc')
test_typing('t/typing/resize-integer.plc')
test_typing('t/typing/size-of-integer.plc')
test_typing('t/typing/int-to-bytestring.plc')
test_typing('t/typing/concatenate.plc')
test_typing('t/typing/take-bytestring.plc')
test_typing('t/typing/sha.plc')
test_typing('t/typing/verify-signature.plc')
test_typing('t/typing/resize-bytestring.plc')
test_typing('t/typing/equals-bytestring.plc')
test_typing('t/typing/txhash.plc')
test_typing('t/typing/blocknum.plc')

test_typing('t/typing/succ-integer.plc')
test_typing('t/typing/unitval.plc')
test_typing('t/typing/true.plc')
test_typing('t/typing/false.plc')
test_typing('t/typing/church-zero.plc')
test_typing('t/typing/church-succ.plc')

test_typing('t/typing/one.plc')
test_typing('t/typing/case.plc')
test_typing('t/typing/verify-identity.plc')
test_typing('t/typing/verify-identity-alt.plc')

# Reachability based tests
# ------------------------

unit_tests  = proj.source('unit-tests.md') \
                  .then(proj.tangle().output(proj.builddir('unit-tests-spec.k')))
lazy_unit   = unit_tests.then(lazy.kprove().ext('lazy')) \
                        .alias('unit-tests') \
                        .default()
# TODO: Needs tangle preprocessing
# strict_unit = unit_tests.then(strict.kprove().ext('strict'))

verification  = proj.source('verification.md') \
                    .then(proj.tangle().output(proj.builddir('verification-spec.k'))) \
                    .then(lazy.kprove().ext('lazy')) \
                    .alias('verification') \

sum_to_10_spec = proj.source('sum-to-10-spec.k') \
                     .then(lazy.kprove().ext('lazy')) \
                     .alias('sum-to-10') \
                     .default()

# TODO: this is not run by default: takes too long
typing_spec = proj.source('typing-tests.md') \
                  .then(proj.tangle().output(proj.builddir('typing-tests-spec.k'))) \
                  .then(typing.kprove().ext('typing')) \
                  .alias('typing-tests')

# Alias' used for convenience
# ---------------------------

proj.build('t/lazy',          'phony', inputs = Target.to_paths(lazy_tests))
proj.build('t/strict',        'phony', inputs = Target.to_paths(strict_tests))
proj.build('t/llvm-strict',  'phony', inputs = Target.to_paths(llvm_strict_tests))
proj.build('t/llvm-lazy',    'phony', inputs = Target.to_paths(llvm_lazy_tests))
proj.build('t/typing',        'phony', inputs = Target.to_paths(typing_tests))

